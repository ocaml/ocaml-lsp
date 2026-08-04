open Lsp.Types
module Position = Lsp.Position

let%expect_test "logical position conversions round-trip" =
  let position = Position.create ~line:2 ~character:3 in
  let line, character = Position.to_logical position in
  Printf.printf
    "to_logical (%d, %d) -> (%d, %d)\n"
    position.line
    position.character
    line
    character;
  let back = Position.of_logical ~line ~character in
  Printf.printf "of_logical -> (%d, %d)\n" back.line back.character;
  [%expect
    {|
    to_logical (2, 3) -> (3, 3)
    of_logical -> (2, 3)
    |}]
;;
