open Test.Import
module Req = Ocaml_lsp_server.Custom_request.Type_expression

module Util = struct
  let call_type_expr ~position ~expression client =
    let text_document =
      TextDocumentIdentifier.create ~uri:(DocumentUri.of_path "test.ml")
    in
    let params =
      Req.Request_params.create ~text_document ~position ~expression
      |> Req.Request_params.yojson_of_t
      |> Jsonrpc.Structured.t_of_yojson
      |> Option.some
    in
    let req = Lsp.Client_request.UnknownRequest { meth = Req.meth; params } in
    Client.request client req
  ;;

  let test ~line ~character ~expression source =
    let position = Position.create ~line ~character in
    let request client =
      let open Fiber.O in
      let+ response = call_type_expr ~position ~expression client in
      Test.print_result response
    in
    Helpers.test source request
  ;;
end

let%expect_test "Type an expression - 1" =
  let source = {| let x = 10 let y = 11 |}
  and line = 0
  and character = 9
  and expression = {|"foo"|} in
  Util.test ~line ~character ~expression source;
  [%expect {| "string" |}]
;;

let%expect_test "Type an expression - 2" =
  let source = {| let x = 10 let y = function `Foo -> () | _ -> () |}
  and line = 1
  and character = 0
  and expression = {|List.map|} in
  Util.test ~line ~character ~expression source;
  [%expect {| "('a -> 'b) -> 'a list -> 'b list" |}]
;;

let%expect_test "Type an expression - with unbound value" =
  let source = {| let x = 10 let y = function `Foo -> () | _ -> () |}
  and line = 1
  and character = 0
  and expression = {|z|} in
  Util.test ~line ~character ~expression source;
  [%expect {| "Unbound value z" |}]
;;

let%expect_test "Type an expression - with menhir error" =
  let source = {| let x = 10 let y = function `Foo -> () | _ -> () |}
  and line = 1
  and character = 0
  and expression = {|('a, ) list|} in
  Util.test ~line ~character ~expression source;
  [%expect {| "Ocaml_preprocess.Parser_raw.MenhirBasics.Error" |}]
;;

let%expect_test "Type a big expression" =
  let source = {| let x = 10 let y = function `Foo -> () | _ -> () |}
  and line = 1
  and character = 0
  and expression = {|List|} in
  Util.test ~line ~character ~expression source;
  [%expect {| "sig\n  type 'a t = 'a list = [] | ( :: ) of 'a * 'a list\n\n  val length : 'a list -> int\n  val compare_lengths : 'a list -> 'b list -> int\n  val compare_length_with : 'a list -> int -> int\n  val is_empty : 'a list -> bool\n  val cons : 'a -> 'a list -> 'a list\n  val singleton : 'a -> 'a list\n  val hd : 'a list -> 'a\n  val tl : 'a list -> 'a list\n  val nth : 'a list -> int -> 'a\n  val nth_opt : 'a list -> int -> 'a option\n  val rev : 'a list -> 'a list\n  val init : int -> (int -> 'a) -> 'a list\n  val append : 'a list -> 'a list -> 'a list\n  val rev_append : 'a list -> 'a list -> 'a list\n  val concat : 'a list list -> 'a list\n  val flatten : 'a list list -> 'a list\n  val equal : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool\n  val compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int\n  val iter : ('a -> unit) -> 'a list -> unit\n  val iteri : (int -> 'a -> unit) -> 'a list -> unit\n  val map : ('a -> 'b) -> 'a list -> 'b list\n  val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list\n  val rev_map : ('a -> 'b) -> 'a list -> 'b list\n  val filter_map : ('a -> 'b option) -> 'a list -> 'b list\n  val concat_map : ('a -> 'b list) -> 'a list -> 'b list\n\n  val fold_left_map :\n    ('acc -> 'a -> 'acc * 'b) ->\n    'acc ->\n    'a list ->\n    'acc * 'b list\n\n  val fold_left :\n    ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc\n\n  val fold_right :\n    ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc\n\n  val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit\n  val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list\n\n  val rev_map2 :\n    ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list\n\n  val fold_left2 :\n    ('acc -> 'a -> 'b -> 'acc) ->\n    'acc ->\n    'a list ->\n    'b list ->\n    'acc\n\n  val fold_right2 :\n    ('a -> 'b -> 'acc -> 'acc) ->\n    'a list ->\n    'b list ->\n    'acc ->\n    'acc\n\n  val for_all : ('a -> bool) -> 'a list -> bool\n  val exists : ('a -> bool) -> 'a list -> bool\n\n  val for_all2 :\n    ('a -> 'b -> bool) -> 'a list -> 'b list -> bool\n\n  val exists2 :\n    ('a -> 'b -> bool) -> 'a list -> 'b list -> bool\n\n  val mem : 'a -> 'a list -> bool\n  val memq : 'a -> 'a list -> bool\n  val find : ('a -> bool) -> 'a list -> 'a\n  val find_opt : ('a -> bool) -> 'a list -> 'a option\n  val find_index : ('a -> bool) -> 'a list -> int option\n  val find_map : ('a -> 'b option) -> 'a list -> 'b option\n\n  val find_mapi :\n    (int -> 'a -> 'b option) -> 'a list -> 'b option\n\n  val filter : ('a -> bool) -> 'a list -> 'a list\n  val find_all : ('a -> bool) -> 'a list -> 'a list\n  val filteri : (int -> 'a -> bool) -> 'a list -> 'a list\n  val take : int -> 'a list -> 'a list\n  val drop : int -> 'a list -> 'a list\n  val take_while : ('a -> bool) -> 'a list -> 'a list\n  val drop_while : ('a -> bool) -> 'a list -> 'a list\n  val partition : ('a -> bool) -> 'a list -> 'a list * 'a list\n\n  val partition_map :\n    ('a -> ('b, 'c) Either.t) -> 'a list -> 'b list * 'c list\n\n  val assoc : 'a -> ('a * 'b) list -> 'b\n  val assoc_opt : 'a -> ('a * 'b) list -> 'b option\n  val assq : 'a -> ('a * 'b) list -> 'b\n  val assq_opt : 'a -> ('a * 'b) list -> 'b option\n  val mem_assoc : 'a -> ('a * 'b) list -> bool\n  val mem_assq : 'a -> ('a * 'b) list -> bool\n  val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list\n  val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list\n  val split : ('a * 'b) list -> 'a list * 'b list\n  val combine : 'a list -> 'b list -> ('a * 'b) list\n  val sort : ('a -> 'a -> int) -> 'a list -> 'a list\n  val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list\n  val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list\n  val sort_uniq : ('a -> 'a -> int) -> 'a list -> 'a list\n\n  val merge :\n    ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list\n\n  val to_seq : 'a list -> 'a Seq.t\n  val of_seq : 'a Seq.t -> 'a list\nend" |}]
;;
