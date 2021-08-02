open Import

type level = MessageType.t =
  | Error
  | Warning
  | Info
  | Log

type t =
  { log_level : level
  ; send_notification : Server_notification.t -> unit Fiber.t
  }

type logger_instance =
  | Uninitialized
  | Initialized of t

let logger_instance = ref Uninitialized

let unwrap_exn logger_instance =
  match !logger_instance with
  | Uninitialized -> Code_error.raise "uninitialized logger used" []
  | Initialized instance -> instance

(* TODO: show_level is for [ShowMessage]s *)
let initialize ~log_level ~show_level:_ ~send_notification : unit =
  logger_instance := Initialized { log_level; send_notification }

let should_log msg_log_level =
  let log_level = (unwrap_exn logger_instance).log_level in
  match (log_level, msg_log_level) with
  | Log, _ -> true
  | Info, (Info | Warning | Error) -> true
  | Info, Log -> false
  | Warning, (Warning | Error) -> true
  | Warning, _ -> false
  | Error, Error -> true
  | Error, _ -> false

let log msg_log_lvl msg_f =
  let { send_notification; _ } = unwrap_exn logger_instance in
  if not (should_log msg_log_lvl) then
    Fiber.return ()
  else
    let log_msg_params =
      LogMessageParams.create ~type_:msg_log_lvl ~message:(msg_f ())
    in
    send_notification (Server_notification.LogMessage log_msg_params)

let log_e f = log Error f

let log_w f = log Warning f

let log_i f = log Info f

let log_l f = log Log f
