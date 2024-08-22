let run_test ~title source =
  let src, range = Code_actions.parse_selection source in
  match Code_actions.apply_code_action_no_edit ~diagnostics:[] title src range with
  | Some () -> print_endline "Action applied successfully"
  | None -> print_endline "No matching target or action could not be applied"
;;

let jump_test = run_test ~title:"Jump to Target"

let%expect_test "jump to unrecognized target" =
  jump_test
    {|
type t = Foo of int | $B$ar of bool
|};
  [%expect {| No matching target or action could not be applied |}]
;;


let%expect_test "jump to a target" =
  jump_test
    {|
type t = Foo of int | Bar of bool
let f (x : t) (d : bool) =
  match x with
  |Bar $x$ -> x
  |Foo _ -> d
|};
  [%expect {|
    {
      "arguments": [
        {
          "uri": "file:///foo.ml",
          "position": { "character": 7, "line": 4 },
          "locations": [
            {
              "end": { "character": 0, "line": 2 },
              "start": { "character": 0, "line": 2 }
            }
          ],
          "multiple": "peek",
          "noResultsMessage": "No targets found"
        }
      ],
      "command": "editor.action.goToLocations",
      "title": "Merlin Jump"
    }
    Action applied successfully
|}]
;;

