(**Try's the parse the incoming string for a prefix. The string should be a
   reversed copy of the line, starting at the position where the prefix ends and
   continuing backwards from there. Does not handle whitespace other than spaces *)
val try_parse_with_regex : string -> string option
