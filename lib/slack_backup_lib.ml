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
  | `ParseFailure e -> Printf.sprintf "Parse failure: %s" e
  | `Rate_limited -> "Rate limit active"
  | `Success _ -> "Success"
  | `Unhandled_error e -> Printf.sprintf "Unhandled error: %s" e
  | `Unknown_error -> "Unknown error"
  | `User_is_bot -> "User is a bot"
  | `User_not_found -> "User is not found"
  | `User_not_visible -> "User is not visible"

let fetch_history fetch =
  let open Yojson.Basic.Util in
  let open Slacko in
  let rec fetch_all ?latest acc =
    match%lwt fetch latest with
    | `Success history_obj ->
      if history_obj.has_more then
        match history_obj.messages with
        | [] ->
          Lwt.return @@ Result.Ok acc
        | m::_ ->
          fetch_all ~latest:m.ts (history_obj.messages @ acc)
      else
        Lwt.return @@ Result.Ok (history_obj.messages @ acc)
    | e -> Lwt.return @@ Result.Error e
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

let conversation_of_user token user =
  let open Yojson.Basic.Util in
  let open Slacko in
  im_list token >|= function
  | `Success im_objs ->
    begin
      try
        let im =
          List.find (fun (im_obj : im_obj) ->
              im_obj.user = user
            ) im_objs
        in
        Result.Ok (conversation_of_string im.id)
      with Not_found ->
        Result.Error `Unknown_error
    end
  | e -> Result.Error e

let verify_token token =
  match%lwt Slacko.auth_test token with
  | `Success _ ->
    Lwt.return @@ Result.Ok ()
  | e ->
    Lwt.return @@ Result.Error e
