open Import

(** This code action allows the user to invoke Merlin-destruct to enumerate
    cases from various lines of a partial match statement. If the line is of any
    of these forms: [match x] [match x with] [| x -> y] then the pre-processing
    will extract [x] and invoke Merlin-destruct on it. Some post-processing is
    applied to Merlin's response to make it more useful for adding subsequent
    code: extraneous tokens are stripped and cases are split across lines. For
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
val t : State.t -> Code_action.t
