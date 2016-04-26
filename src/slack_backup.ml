module SB = Slack_backup_lib

type selector =
  [ `Id of string
  | `Name of string
  ]

let selector_to_string = function
  | (`Id s | `Name s) -> s

let conversation_of_selector token name =
  match name with
  | `Id id ->
    Lwt.return @@ Result.Ok (Slacko.conversation_of_string id)
  | `Name user_name ->
    let user = Slacko.user_of_string user_name in
    SB.conversation_of_user token user

let channel_of_selector token selector =
  try%lwt
    let channel = selector |> selector_to_string |> Slacko.channel_of_string in
    Lwt.return @@ Result.Ok channel
  with e ->
    Lwt.return @@ Result.Error (`Unhandled_error (Printexc.to_string e))

let get_conversation token conversation_selector =
  let open Lwt in
  Lwt_main.run begin
    match%lwt conversation_of_selector token conversation_selector with
    | Result.Error e ->
      Printf.printf "Error while looking for the conversation %s: %s\n"
        (selector_to_string conversation_selector)
        (SB.error_to_string e);
      return ()
    | Result.Ok conversation ->
      SB.fetch_conversation ~count:1000 token conversation >|= function
      | Result.Error e ->
        Printf.printf "Error while fetching the conversation: %s\n"
          (SB.error_to_string e)
      | Result.Ok json ->
        Printf.printf "%s\n" (Yojson.Basic.pretty_to_string json)
  end

let get_channel token channel_selector =
  let open Lwt in
  Lwt_main.run begin
    match%lwt channel_of_selector token channel_selector with
    | Result.Error e ->
      Printf.printf "Error while looking for the channel %s: %s\n"
        (selector_to_string channel_selector)
        (SB.error_to_string e);
      return ()
    | Result.Ok channel ->
      SB.fetch_channel ~count:1000 token channel >|= function
      | Result.Error e ->
        Printf.printf "Error while fetching the channel: %s\n"
          (SB.error_to_string e)
      | Result.Ok json ->
        Printf.printf "%s\n" (Yojson.Basic.pretty_to_string json)
  end

let list_users token =
  let open Lwt in
  Lwt_main.run begin
    Slacko.users_list token >|= fun c ->
    match c with
    | `Success json ->
      Yojson.Basic.pretty_to_string json |> Printf.printf "%s\n"
    | e ->
      Printf.printf "Error while fetching the list of users: %s\n"
        (SB.error_to_string e)
  end

let list_channels token exclude_archived =
  let open Lwt in
  Lwt_main.run begin
    Slacko.channels_list ~exclude_archived token >|= fun c ->
    match c with
    | `Success json ->
      Yojson.Basic.pretty_to_string json |> Printf.printf "%s\n"
    | e ->
      Printf.printf "Error while fetching the list of channels: %s\n"
        (SB.error_to_string e)
  end

let list_conversations token =
  let open Lwt in
  Lwt_main.run begin
    Slacko.im_list token >|= fun c ->
    match c with
    | `Success json ->
      Yojson.Basic.pretty_to_string json |> Printf.printf "%s\n"
    | e ->
      Printf.printf "Error while fetching the list of conversations: %s\n"
        (SB.error_to_string e)
  end

open Cmdliner

let verify_token =
  let parser_val token =
    let t = Slacko.token_of_string token in
    Lwt_unix.run begin
      match%lwt SB.verify_token t with
      | Result.Ok () ->
        Lwt.return @@ `Ok (t)
      | Result.Error e ->
        let msg =
          Printf.sprintf "Error while checking the token: %s"
            (SB.error_to_string e)
        in
        Lwt.return @@ `Error msg
    end
  in
  let printer_val fmt token =
    Format.fprintf fmt "%s" "secret-valid-token"
  in
  parser_val, printer_val

let token =
  let doc = "Slack API token. Can be generated from https://api.slack.com/docs/oauth-test-tokens" in
  let env = Arg.env_var "SLACK_TOKEN" ~doc in
  let info' = Arg.info ~docv:"SLACK_TOKEN" ~doc ~env ["t"; "token"] in
  Arg.(required & opt (some verify_token) None info')

let channel_name =
  let doc = "Name of the channel you want to backup. Must start with #" in
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

let conversation =
  let info = Term.info "conversation" ~doc:"Archive an IM conversation" in
  let term = Term.(const get_conversation $ token $ conversation_selector) in
  term, info

let channel =
  let info = Term.info "channel" ~doc:"Archive a channel conversation" in
  let term = Term.(const get_channel $ token $ channel_selector) in
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
          [ conversation
          ; channel
          ; list_users
          ; list_channels
          ; list_conversations
          ]
  with
  | `Error _ -> exit 1
  | _ -> exit 0
