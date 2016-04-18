let error_to_string = function
  | `Channel_not_found -> "Channel unknown"
  | `Invalid_auth -> "Invalid token"
  | `Invalid_ts_latest -> "Invalid_ts_latest"
  | `Invalid_ts_oldest -> "Invalid_ts_oldest"
  | `Is_archived -> "Channel is archived"
  | `Msg_too_long -> "Message too long"
  | `Not_authed -> "Not_authed"
  | `Rate_limited -> "Rate limit active"
  | `Unhandled_error e -> Printf.sprintf "Unhandled error: %s" e
  | `Unknown_error
  | _ -> "Unknown error"

let fetch_history ?latest ?count token history channel =
  let open Lwt in
  let open Yojson.Basic.Util in
  let rec fetch ?latest acc =
    history token ?latest ?oldest:None ?count channel >>= (fun history ->
        match history with
        | `Success json ->
          let has_more = json |> member "has_more" |> to_bool in
          let messages = json |> member "messages" |> to_list |> List.rev in
          if has_more then
            match messages with
            | [] -> return acc
            | m::_ ->
              let latest_ts = m |> member "ts" |> to_string |> float_of_string in
              fetch ~latest:latest_ts (messages @ acc)
          else
            return (messages @ acc)
        | _ -> return acc       (* TODO: log error? *)
      )
  in
  fetch ?latest []

let user_id_of_name token name =
  let open Lwt in
  let open Yojson.Basic.Util in
  Slacko.users_list token >|= function
  | `Success json ->
    let users = json |> member "members" |> to_list in
    begin try
        let id =
          List.find (fun user_json ->
              let n = user_json |> member "name" |> to_string in
              name = n
            ) users
          |> member "id"
          |> to_string
        in
        Some id
      with Not_found ->
        None
    end
  | _ -> None

let conversation_id_of_selector token =
  let open Lwt in
  let open Yojson.Basic.Util in
  function
  | `Id id -> return (Some id)
  | `Name name ->
    user_id_of_name token name >>= function
    | None -> return None
    | Some user_id ->
      Slacko.im_list token >|= function
      | `Success json ->
        let conversations = json |> member "ims" |> to_list in
        begin try
            let id =
              List.find (fun disc_json ->
                  let u = disc_json |> member "user" |> to_string in
                  user_id = u
                ) conversations
              |> member "id"
              |> to_string
            in
            Some id
          with Not_found ->
            None
        end
      | _ -> None

let channel_id_of_selector token =
  let open Lwt in
  let open Yojson.Basic.Util in
  function
  | `Id id -> return (Some id)
  | `Name name ->
    Slacko.channels_list token >|= function
    | `Success json ->
      let channels = json |> member "channels" |> to_list in
      begin try
          let id =
            List.find (fun chan_json ->
                let n = chan_json |> member "name" |> to_string in
                name = n
              ) channels
            |> member "id"
            |> to_string
          in
          Some id
        with Not_found ->
          None
      end
    | _ -> None

let get_conversation token conversation_selector count =
  let open Lwt in
  let token = Slacko.token_of_string token in
  Lwt_main.run begin
    conversation_id_of_selector token conversation_selector >>= fun channel_id ->
    match channel_id with
    | None ->
      return @@ Printf.printf "Unable to find the id for this conversation.\n"
    | Some channel_id ->
      let conversation = Slacko.conversation_of_string channel_id in
      fetch_history ?count token Slacko.im_history conversation >|= (fun json ->
          Printf.printf "%s\n" (Yojson.Basic.pretty_to_string (`List json))
        )
  end

let get_channel token channel_selector count =
  let open Lwt in
  let token = Slacko.token_of_string token in
  Lwt_main.run begin
    channel_id_of_selector token channel_selector >>= fun channel_id ->
    match channel_id with
    | None ->
      return @@ Printf.printf "Unable to find the id for the given channel.\n"
    | Some channel_id ->
      let channel = Slacko.channel_of_string channel_id in
      fetch_history ?count token Slacko.channels_history channel >|= (fun json ->
          Printf.printf "%s\n" (Yojson.Basic.pretty_to_string (`List json))
        )
  end

let list_users token =
  let open Lwt in
  let token = Slacko.token_of_string token in
  Slacko.users_list token >|= (fun c ->
      match c with
      | `Success json -> Yojson.Basic.pretty_to_string json |> Printf.printf "%s\n"
      | e -> Printf.printf "%s\n" (error_to_string e)
    )
  |> Lwt_main.run

let list_channels token exclude_archived =
  let open Lwt in
  let token = Slacko.token_of_string token in
  Slacko.channels_list ~exclude_archived token >|= (fun c ->
      match c with
      | `Success json -> Yojson.Basic.pretty_to_string json |> Printf.printf "%s\n"
      | e -> Printf.printf "%s\n" (error_to_string e)
    )
  |> Lwt_main.run

let list_conversations token =
  let open Lwt in
  let token = Slacko.token_of_string token in
  Slacko.im_list token >|= (fun c ->
      match c with
      | `Success json -> Yojson.Basic.pretty_to_string json |> Printf.printf "%s\n"
      | e -> Printf.printf "%s\n" (error_to_string e)
    )
  |> Lwt_main.run

open Cmdliner

let token =
  let doc = "The Slack API access token" in
  let env = Arg.env_var "SLACK_TOKEN" ~doc in
  Arg.(required
       & opt (some string) None
       & info ["t"; "token"] ~env ~docv:"SLACK_TOKEN" ~doc
      )

let channel_name =
  let doc = "Name of the channel you want to backup" in
  Arg.(value & opt (some string) None & info ["name"] ~docv:"CHANNEL_NAME" ~doc)

let channel_id =
  let doc = "ID of the channel you want to backup" in
  Arg.(value & opt (some string) None & info ["id"] ~docv:"CHANNEL_ID" ~doc)

let user_name =
  let doc = "Name of the user for which you want to backup the conversation" in
  Arg.(value & opt (some string) None & info ["name"] ~docv:"USER_NAME" ~doc)

let conversation_id =
  let doc = "ID of the conversation you want to backup" in
  Arg.(value & opt (some string) None & info ["id"] ~docv:"CONVERSATION_ID" ~doc)

let discussion_kind name id =
  match name, id with
  | Some n, None -> `Ok (`Name n)
  | None, Some id -> `Ok (`Id id)
  | Some _, Some _ ->
    `Error (
      false,
      Printf.sprintf
        "Options --name and --id are exclusive. Only one at a time can be used."
    )
  | None, None ->
    `Error (
      false,
      Printf.sprintf "--name or --id must be given."
    )

let conversation_selector =
  Term.(ret (const discussion_kind $ user_name $ conversation_id))

let channel_selector =
  Term.(ret (const discussion_kind $ channel_name $ channel_id))

let count =
  let doc = "Number of messages to get in each call to the slack API (must be between 1 and 1000)" in
  Arg.(value & opt (some int) (Some 100) & info ["count"] ~docv:"COUNT" ~doc) (* TODO: convertor min max *)

let conversation =
  let info = Term.info "conversation" ~doc:"Archive an IM conversation" in
  let term = Term.(const get_conversation $ token $ conversation_selector $ count) in
  term, info

let channel =
  let info = Term.info "channel" ~doc:"Archive a channel conversation" in
  let term = Term.(const get_channel $ token $ channel_selector $ count) in
  term, info

let list_users =
  let doc = "Display the list of users" in
  let info = Term.info "list-users" ~doc in
  let term = Term.(const list_users $ token) in
  term, info

let exclude_archived =
  let doc = "Exclude archived channels from the list" in
  Arg.(value & flag & info ["exclude-archived"] ~docv:"EXCLUDE_ARCHIVED" ~doc)

let list_channels =
  let doc = "Display the list of channels" in
  let info = Term.info "list-channels" ~doc in
  let term = Term.(const list_channels $ token $ exclude_archived) in
  term, info

let list_conversations =
  let doc = "Display the list of IM conversations" in
  let info = Term.info "list-conversations" ~doc in
  let term = Term.(const list_conversations $ token) in
  term, info

let help =
  let doc = "Slack backup tool." in
  Term.(ret (const (fun _ -> `Help (`Pager, None)) $ choice_names)),
  Term.info "slack-backup" ~doc ~version:"0.1.0"

let () =
  match Term.eval_choice ~catch:true help
          [conversation; channel; list_users; list_channels; list_conversations]
  with
  | `Error _ -> exit 1
  | _ -> exit 0
