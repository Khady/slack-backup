open Lwt.Infix

let error_to_string = function
  | `Account_inactive -> "Accound is inactive"
  | `Channel_not_found -> "Channel unknown"
  | `Invalid_auth -> "Invalid token"
  | `Invalid_ts_latest -> "Invalid_ts_latest"
  | `Invalid_ts_oldest -> "Invalid_ts_oldest"
  | `Is_archived -> "Channel is archived"
  | `Msg_too_long -> "Message too long"
  | `Not_authed -> "Not_authed"
  | `Rate_limited -> "Rate limit active"
  | `Success _ -> "Success"
  | `Unhandled_error e -> Printf.sprintf "Unhandled error: %s" e
  | `Unknown_error -> "Unknown error"
  | `User_not_found -> "User is not found"
  | `User_not_visible -> "User is not visible"

let fetch_history fetch =
  let open Yojson.Basic.Util in
  let to_json_list j = `List j in
  let rec fetch_all ?latest acc =
    match%lwt fetch latest with
    | `Success json ->
      let has_more = json |> member "has_more" |> to_bool in
      let messages = json |> member "messages" |> to_list |> List.rev in
      if has_more then
        match messages with
        | [] ->
          Lwt.return @@ Result.Ok (to_json_list acc)
        | m::_ ->
          let latest_ts = m |> member "ts" |> to_string |> float_of_string in
          fetch_all ~latest:latest_ts (messages @ acc)
      else
        Lwt.return @@ Result.Ok (to_json_list @@ messages @ acc)
    | e -> Lwt.return @@ Result.Error e
    | exception e ->
      Lwt.return @@ Result.Error (`Unhandled_error (Printexc.to_string e))
  in
  fetch_all []

let fetch_conversation ?latest ?oldest ?count token conversation =
  fetch_history
    (fun l ->
       let latest =
         match l with
         | None -> latest
         | Some l -> Some l
       in
       Slacko.im_history ?latest ?oldest ?count token conversation
    )

let fetch_channel ?latest ?oldest ?count token channel =
  fetch_history
    (fun l ->
       let latest =
         match l with
         | None -> latest
         | Some l -> Some l
       in
       Slacko.channels_history ?latest ?oldest ?count token channel
    )

let string_id_of_user token user =
  let open Yojson.Basic.Util in
  try%lwt
    Slacko.users_info token user >|= function
    | `Success json ->
      let id = json |> member "user" |> member "id" |> to_string in
      Result.Ok id
    | e -> Result.Error e
  with e ->
    (* TODO: Treat Slacko.No_matches as a specific case *)
    Lwt.return @@ Result.Error (`Unhandled_error (Printexc.to_string e))

let conversation_of_user token user =
  let open Yojson.Basic.Util in
  match%lwt string_id_of_user token user with
  | (Result.Error _) as e -> Lwt.return e
  | Result.Ok user_id ->
    Slacko.im_list token >|= function
    | `Success json ->
      let conversations = json |> member "ims" |> to_list in
      begin
        try
          Result.Ok (
            List.find (fun disc_json ->
                let u = disc_json |> member "user" |> to_string in
                user_id = u
              ) conversations
            |> member "id"
            |> to_string
            |> Slacko.conversation_of_string
          )
        with Not_found ->
          Result.Error `Channel_not_found
      end
    | e -> Result.Error e

let verify_token token =
  match%lwt Slacko.auth_test token with
  | `Success _ ->
    Lwt.return @@ Result.Ok ()
  | e ->
    Lwt.return @@ Result.Error e
  | exception e ->
    Lwt.return @@ Result.Error (`Unhandled_error (Printexc.to_string e))
