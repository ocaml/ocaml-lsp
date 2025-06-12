open Import

(** If the cursor is on a function symbol, we resolve the reference of that function and
    use it as the base for the call hierarchy. If the cursor is on any other place, we
    find next closest parent function and use it for the call hierarchy. *)
val handle_prepare
  :  log_info:Lsp_timing_logger.t
  -> State.t Server.t
  -> CallHierarchyPrepareParams.t
  -> CallHierarchyItem.t list option Fiber.t

val handle_incoming
  :  log_info:Lsp_timing_logger.t
  -> State.t Server.t
  -> CallHierarchyIncomingCallsParams.t
  -> CallHierarchyIncomingCall.t list option Fiber.t

(* TODO: [handle_outgoing] is currently unimplemented. *)
val handle_outgoing
  :  log_info:Lsp_timing_logger.t
  -> State.t Server.t
  -> CallHierarchyOutgoingCallsParams.t
  -> CallHierarchyOutgoingCall.t list option Fiber.t
