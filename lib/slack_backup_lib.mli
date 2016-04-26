val error_to_string :
  [< `Account_inactive
  | `Channel_not_found
  | `Invalid_auth
  | `Invalid_ts_latest
  | `Invalid_ts_oldest
  | `Is_archived
  | `Msg_too_long
  | `Not_authed
  | `Rate_limited
  | `Success of 'a
  | `Unhandled_error of string
  | `Unknown_error
  | `User_not_found
  | `User_not_visible
  ] ->
  string
(** Convert a slacko error to a string. *)

val fetch_history :
  (float option -> Slacko.history_result Lwt.t) ->
  (Yojson.Basic.json, Slacko.history_result) Result.result Lwt.t

val fetch_conversation :
  ?latest:Slacko.timestamp ->
  ?oldest:Slacko.timestamp ->
  ?count:int ->
  Slacko.token ->
  Slacko.conversation ->
  (Yojson.Basic.json, Slacko.history_result) Result.result Lwt.t

val fetch_channel :
  ?latest:Slacko.timestamp ->
  ?oldest:Slacko.timestamp ->
  ?count:int ->
  Slacko.token ->
  Slacko.channel ->
  (Yojson.Basic.json, Slacko.history_result) Result.result Lwt.t

val string_id_of_user :
  Slacko.token ->
  Slacko.user ->
  (string,
   [> Slacko.authed_result | Slacko.user_error | Slacko.user_visibility_error ]
  ) Result.result Lwt.t

val conversation_of_user :
  Slacko.token ->
  Slacko.user ->
  (Slacko.conversation,
   [> Slacko.authed_result
   | Slacko.user_error
   | Slacko.user_visibility_error
   | Slacko.channel_error
   ]
  ) Result.result Lwt.t

val verify_token :
  Slacko.token ->
  (unit, [> Slacko.authed_result ]) Result.result Lwt.t
