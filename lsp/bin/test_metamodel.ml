module Metamodel = Lsp_gen.Metamodel

let file = Sys.argv.(1)

let () =
  let read = open_in file in
  let s = really_input_string read (in_channel_length read) in
  let json = Yojson.Safe.from_string s in
  let (_ : Metamodel.t) = Metamodel.t json in
  close_in read
;;
