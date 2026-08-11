open Test.Import

let document_link ?(uri = Helpers.uri) client =
  let textDocument = TextDocumentIdentifier.create ~uri in
  Client.request client (TextDocumentLink (DocumentLinkParams.create ~textDocument ()))
;;

let print_link (link : DocumentLink.t) =
  let target =
    Option.map link.target ~f:(fun target ->
      match DocumentUri.to_string target |> String.chop_prefix ~prefix:"file://" with
      | None -> DocumentUri.to_string target
      | Some path -> Filename.basename path)
  in
  DocumentLink.yojson_of_t
    (DocumentLink.create
       ~range:link.range
       ?target:(Option.map target ~f:DocumentUri.of_string)
       ?tooltip:link.tooltip
       ())
;;

let print_links = Test.print_option_list ~none:"null" print_link

let test source =
  let req client =
    let* response = document_link client in
    print_links response;
    Fiber.return ()
  in
  Helpers.test source req
;;

(* Cross-references carry no target until the client asks for one. *)
let test_resolved ?uri source =
  let req client =
    let* response = document_link ?uri client in
    let* resolved =
      Fiber.sequential_map (Option.value response ~default:[]) ~f:(fun link ->
        Client.request client (TextDocumentLinkResolve link))
    in
    print_links (Some resolved);
    Fiber.return ()
  in
  Helpers.test ?uri source req
;;

let%expect_test "links the url of an odoc link" =
  test
    {|(** See {{:https://ocaml.org} the website}. *)
let x = 1|};
  [%expect {| null |}]
;;

let%expect_test "links the url of a @see tag" =
  test
    {|(** Does nothing.

    @see <https://ocaml.org/manual> the manual *)
let x = 1|};
  [%expect {| null |}]
;;

let%expect_test "preserves the query and fragment of a url" =
  test
    {|(** See {{:https://ocaml.org/p?q=1#top} releases}. *)
let x = 1|};
  [%expect {| null |}]
;;

let%expect_test "finds links nested in markup" =
  test
    {|(** {2 A {{:https://ocaml.org} heading}}

    - a {b bold {{:https://opam.ocaml.org} item}} *)
let x = 1|};
  [%expect {| null |}]
;;

let%expect_test "links odoc cross-references" =
  test
    {|type t = Foo

(** Know if a value of type {!t} is {!Foo}. *)
let is_foo Foo = true|};
  [%expect {| null |}]
;;

let%expect_test "resolves a cross-reference to an earlier definition" =
  test_resolved
    {|type t = Foo

(** Know if a value of type {!t} is {!Foo}. *)
let is_foo Foo = true
|};
  [%expect {| [] |}]
;;

let%expect_test "resolves a cross-reference within a recursive group" =
  test_resolved
    {|(** Built on {!helper}. *)
let rec main () = helper ()

and helper () = ()

let other = 1
|};
  [%expect {| [] |}]
;;

let%expect_test "resolves a cross-reference qualified by its kind" =
  test_resolved
    {|type t = Foo

let is_foo Foo = true

(** See {!type:t} and {!val:is_foo}. *)
let other = 1
|};
  [%expect {| [] |}]
;;

(* The [t] below is [M.t], not the one at the end of the file: a reference
   resolves against the signature enclosing it, as it does under odoc. *)
let%expect_test "resolves a reference shadowed by an outer name" =
  test_resolved
    {|module M = struct
  (** this is {!t} *)
  type t = A
end

type t = B
|};
  [%expect {| [] |}]
;;

let%expect_test "resolves a forward cross-reference" =
  test_resolved
    {|let unrelated = 0

(** Applies {!f}. *)
let f x = x
|};
  [%expect {| [] |}]
;;

(* The odoc convention puts the comment after the item it documents. *)
let%expect_test "resolves a reference to the item just above it" =
  test_resolved
    ~uri:(DocumentUri.of_path "test.mli")
    {|type t

val helper : t -> t
(** Uses {!helper} and {!t}. *)
|};
  [%expect {| [] |}]
;;

let%expect_test "leaves an unknown cross-reference without a target" =
  test_resolved
    {|(** Refers to {!Nonexistent.thing}. *)
let x = 1
|};
  [%expect {| [] |}]
;;

let%expect_test "ignores ordinary comments" =
  test
    {|(* {{:https://ocaml.org} not a doc comment} *)
let x = 1|};
  [%expect {| null |}]
;;
