open Import

(** {v
 The combine-cases code action allows the user to select several one-line cases of a
    match and combine them into a single one-line case. If there is a unique non-empty
    right-hand-side expression it will be preserved; otherwise the RHS will be a hole.

    For example, if the user highlights the King and Queen lines of the following snippet
    and invokes combine-cases...
    [ match card with
      | Ace -> _
      | King -> _
      | Queen -> "Face card!"
      | Jack -> "Face card?"
      | Number _ -> _]
    then the code action will update the snippet to:
    [ match card with
      | Ace -> _
      | King | Queen -> "Face card!"
      | Jack -> "Face card?"
      | Number _ -> _]
    If instead (or afterwards) they invoke combine-cases highlighting the King, Queen,
    and Jack lines, they'll get:
    [ match card with
      | Ace -> _
      | King | Queen | Jack -> _
      | Number _ -> _]

    The intended use-case is immediately following a destruct-line code action, which
    produces lots of empty cases on separate lines; this allows several related cases to
    be quickly combined.

    Combine-cases avoids invoking Merlin and instead just works by regular expression
    matching to detect the "|" and "->" tokens. This means it doesn't handle multi-line
    cases and doesn't do anything smart when combining cases.
    v} *)

val kind : CodeActionKind.t
val t : Code_action.t
