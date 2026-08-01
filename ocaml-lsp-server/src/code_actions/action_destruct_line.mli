open Import

(** This code action allows the user to invoke Merlin-destruct to enumerate
    cases from various lines of a partial match statement. If a line contains
    one of these forms: [match x] [match x with] [| x -> y] then the
    pre-processing will extract [x] and invoke Merlin-destruct on it. Existing
    cases are reused when the action is requested on [match]. Merlin's response
    is post-processed to make it more useful for adding subsequent code:
    extraneous tokens are stripped and cases are split across lines. For
    example, supposing [x] is a [bool], then the line [match x with] expands to
    [match x with
     | false -> _
     | true -> _]. The same expansion
    results from invoking the code action on the second line of
    [match x with
     | false -> _].

    In addition, the code action detects a sub-case of the [| x -> y] form,
    where the cursor is on an underscore within [x]. This often corresponds to a
    wildcard pattern where a destruct action is useful and extra post-processing
    helps. The follwing expansions result from repeated applications of
    [destruct-line]:
    [let zip (type a b) (xs : a list) (ys : b list) : (a * b) list =
       match xs, ys]
    (code action invoked anywhere on the match line)
    [let zip (type a b) (xs : a list) (ys : b list) : (a * b) list =
       match (xs, ys) with
       | (_, _) -> _]
    (CA invoked on the first underscore)
    [let zip (type a b) (xs : a list) (ys : b list) : (a * b) list =
       match (xs, ys) with
       | ([], _) -> _
       | (_::_, _) -> _]
    (CA invoked on the first underscore)
    [let zip (type a b) (xs : a list) (ys : b list) : (a * b) list =
       match (xs, ys) with
       | ([], []) -> _
       | ([], _::_) -> _
       | (_::_, _) -> _]
    (CA invoked on the second-to-last underscore)
    [let zip (type a b) (xs : a list) (ys : b list) : (a * b) list =
       match (xs, ys) with
       | ([], []) -> _
       | ([], _::_) -> _
       | (_::_, []) -> _
       | (_::_, _::_) -> _] *)

val kind : CodeActionKind.t
val t : dispatch:Action_destruct.dispatch -> State.t -> Code_action.t

module Testing : sig
  module Search : sig
    type t =
      { match_start : int
      ; case_start : int option
      }

    (** Locate a [match] on the first line and its first case, if any. The [match]
    must either be the first token or contain [position]. *)
    val find : string -> position:int -> t option
  end
end
