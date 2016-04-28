val error_to_string :
  [< `Account_inactive
  | `Channel_not_found
  | `Invalid_auth
  | `Invalid_ts_latest
  | `Invalid_ts_oldest
  | `Is_archived
  | `Msg_too_long
  | `Not_authed
  | `ParseFailure of string
  | `Rate_limited
  | `Success of 'a
  | `Unhandled_error of string
  | `Unknown_error
  | `User_is_bot
  | `User_not_found
  | `User_not_visible ] ->
  string
(** Convert a slacko error to a string. *)

val fetch_history :
  (float option -> Slacko.history_result Lwt.t) ->
  (Slacko.message_obj list, Slacko.history_result) Result.result Lwt.t

val fetch_conversation :
  ?latest:Slacko.timestamp ->
  ?oldest:Slacko.timestamp ->
  ?count:int ->
  Slacko.token ->
  Slacko.conversation ->
  (Slacko.message_obj list, Slacko.history_result) Result.result Lwt.t

val fetch_channel :
  ?latest:Slacko.timestamp ->
  ?oldest:Slacko.timestamp ->
  ?count:int ->
  Slacko.token ->
  Slacko.channel ->
  (Slacko.message_obj list, Slacko.history_result) Result.result Lwt.t

val conversation_of_user :
  Slacko.token ->
  Slacko.user ->
  (Slacko.conversation,
   [ Slacko.parsed_auth_error
   | Slacko.bot_error
   | `Success of Slacko.im_obj list
   ]
  ) Result.result Lwt.t

val verify_token :
  Slacko.token ->
  (unit,
   [ `Success of Slacko.authed_obj
   | Slacko.parsed_auth_error
   ]
  ) Result.result Lwt.t
