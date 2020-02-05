type direction = R | L

type t =
  | Const
  | Null
  | Extends
  | Export
  | Interface
  | Namespace
  | Type
  | Curly of direction
  | Square of direction
  | Angle of direction
  | Paren of direction
  | Semicolon
  | Comma
  | Ident of string
  | Colon
  | Alt
  | Float of float
  | Int of int
  | String of string
  | Array_type
  | Question
  | Equal
