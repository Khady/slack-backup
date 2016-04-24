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

let fetch_history fetch =
  let open Lwt in
  let open Yojson.Basic.Util in
  let to_json_list j = `List j in
  let rec fetch_all ?latest acc =
    fetch latest >>= (fun history ->
        match history with
        | `Success json ->
          let has_more = json |> member "has_more" |> to_bool in
          let messages = json |> member "messages" |> to_list |> List.rev in
          if has_more then
            match messages with
            | [] -> return @@ Result.Ok (to_json_list acc)
            | m::_ ->
              let latest_ts = m |> member "ts" |> to_string |> float_of_string in
              fetch_all ~latest:latest_ts (messages @ acc)
          else
            return @@ Result.Ok (to_json_list @@ messages @ acc)
        | e -> return (Result.Error e)
      )
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
