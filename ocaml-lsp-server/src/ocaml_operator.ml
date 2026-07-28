open Import

let symbolic_characters = {|$&*+-/=>@^|~!?%<:.#|}
let is_symbolic_character = String.mem symbolic_characters
