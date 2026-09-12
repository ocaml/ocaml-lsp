open Test.Import

let inline_test ?print_none source =
  Code_actions.code_action_test ?print_none ~title:"Inline into uses" source
;;

(* Execute assertions in both versions as well as snapshotting the edit. *)
let inline_runtime_test source =
  let source, range = Code_actions.parse_selection source in
  let result =
    Code_actions.apply_code_action "Inline into uses" source range |> Option.value_exn
  in
  let dir = Test.temp_dir "ocamllsp-inline-runtime-" in
  Fun.protect
    ~finally:(fun () -> Test.run_command ("rm -rf -- " ^ Filename.quote dir))
    (fun () ->
       List.iter [ source; result ] ~f:(fun source ->
         Test.write_file (Filename.concat dir "program.ml") source;
         Test.run_command
           ~cwd:dir
           "ocamlc -w -a -o program.exe program.ml && ./program.exe");
       print_string result)
;;

(* Repro: the partial application retains a redundant [self] parameter instead
   of becoming [fun (case : int) -> self.visit self case]. The fully applied use
   below is reduced correctly. *)
let%expect_test "inline partial application retains a redundant parameter" =
  inline_test
    {|
type iterator = { visit : iterator -> int -> unit }
let iter cases ~f = List.iter f cases
let $function_case (self : iterator) (case : int) = self.visit self case
let function_body (self : iterator) cases =
  iter cases ~f:(function_case self)
let direct self case = function_case self case
|};
  [%expect
    {|
    type iterator = { visit : iterator -> int -> unit }
    let iter cases ~f = List.iter f cases
    let function_case (self : iterator) (case : int) = self.visit self case
    let function_body (self : iterator) cases =
      iter cases ~f:((fun (self : iterator) (case : int) -> self.visit self case) self)
    let direct self case = (self.visit self case)
    |}]
;;

let%expect_test "inline partial applications with matching parameter names" =
  inline_test
    {|
let $f x y z = x + y + z
let one x = f x
let two x y = f x y
|};
  [%expect
    {|
    let f x y z = x + y + z
    let one x = ((fun x y z -> (x + y) + z) x)
    let two x y = ((fun x y z -> (x + y) + z) x y)
    |}]
;;

let%expect_test "partial inlining preserves parameter type annotations" =
  inline_runtime_test
    {|
type first = { value : int }
type second = { value : int; extra : unit }
let $f (self : first) ignored = self.value
let g self = f self
let () = assert (g ({ value = 3 } : first) () = 3)
|};
  [%expect
    {|
    type first = { value : int }
    type second = { value : int; extra : unit }
    let f (self : first) ignored = self.value
    let g self = ((fun (self : first) ignored -> self.value) self)
    let () = assert (g ({ value = 3 } : first) () = 3)
    |}]
;;

let%expect_test "partial inlining preserves capture and argument evaluation" =
  inline_runtime_test
    {|
type cell = { mutable value : int }
let $f x y = let outer = y + 1 in x - outer
let y = 10
let outer = 20
let captured = f y
let nested = f outer
let calls = ref 0
let effectful = f (incr calls; 30)
let cell = { value = 40 }
let snapshot = f cell.value
let raised = try ignore (f (failwith "argument")); false with Failure _ -> true
let () =
  assert (!calls = 1);
  cell.value <- 100;
  assert (captured 2 = 7);
  assert (nested 2 = 17);
  assert (effectful 2 = 27);
  assert (effectful 3 = 26);
  assert (!calls = 1);
  assert (snapshot 2 = 37);
  assert raised
|};
  [%expect
    {|
    type cell = { mutable value : int }
    let f x y = let outer = y + 1 in x - outer
    let y = 10
    let outer = 20
    let captured = ((fun x y -> let outer = y + 1 in x - outer) y)
    let nested = ((fun x y -> let outer = y + 1 in x - outer) outer)
    let calls = ref 0
    let effectful = ((fun x y -> let outer = y + 1 in x - outer) (incr calls; 30))
    let cell = { value = 40 }
    let snapshot = ((fun x y -> let outer = y + 1 in x - outer) cell.value)
    let raised = try ignore ((fun x y -> let outer = y + 1 in x - outer) (failwith "argument")); false with Failure _ -> true
    let () =
      assert (!calls = 1);
      cell.value <- 100;
      assert (captured 2 = 7);
      assert (nested 2 = 17);
      assert (effectful 2 = 27);
      assert (effectful 3 = 26);
      assert (!calls = 1);
      assert (snapshot 2 = 37);
      assert raised
    |}]
;;

let%expect_test "partial inlining preserves multiple argument scope and order" =
  inline_runtime_test
    {|
let $f x y z = x - y + z
let x = 3
let y = 10
let swapped = f y x
let log = ref []
let argument tag value = log := !log @ [tag]; value
let effectful = f (argument 1 10) (argument 2 3)
let () =
  assert (swapped 0 = 7);
  assert (!log = [2; 1]);
  assert (effectful 1 = 8);
  assert (effectful 2 = 9);
  assert (!log = [2; 1])
|};
  [%expect
    {|
    let f x y z = x - y + z
    let x = 3
    let y = 10
    let swapped = ((fun x y z -> (x - y) + z) y x)
    let log = ref []
    let argument tag value = log := !log @ [tag]; value
    let effectful = ((fun x y z -> (x - y) + z) (argument 1 10) (argument 2 3))
    let () =
      assert (swapped 0 = 7);
      assert (!log = [2; 1]);
      assert (effectful 1 = 8);
      assert (effectful 2 = 9);
      assert (!log = [2; 1])
    |}]
;;

let%expect_test "partial inlining leaves refutable parameter matching in the function" =
  inline_runtime_test
    {|
let $f (Some x) y = x + y
let deferred = f None
let () =
  assert (try ignore (deferred 1); false with Match_failure _ -> true)
|};
  [%expect
    {|
    let f (Some x) y = x + y
    let deferred = ((fun (Some x) y -> x + y) None)
    let () =
      assert (try ignore (deferred 1); false with Match_failure _ -> true)
    |}]
;;

let%expect_test "inline partial application with duplicate parameter names" =
  inline_runtime_test
    {|
let $f x x z = x + z
let partial = f 1 2
let () = assert (partial 3 = 5)
|};
  [%expect
    {|
    let f x x z = x + z
    let partial = ((fun x x z -> x + z) 1 2)
    let () = assert (partial 3 = 5)
    |}]
;;

let%expect_test "partial inlining retains labelled parameters" =
  inline_runtime_test
    {|
let $f x ~y = x + y
let partial = f 1
let () = assert (partial ~y:2 = 3)
|};
  [%expect
    {|
    let f x ~y = x + y
    let partial = ((fun x ~y -> x + y) 1)
    let () = assert (partial ~y:2 = 3)
    |}]
;;

let%expect_test "inline a shorthand function used as a value and a labelled argument" =
  inline_test
    {|
let use ~f = f 0
let $f x = x + 1
let g = f
let h = use ~f
|};
  [%expect
    {|
    let use ~f = f 0
    let f x = x + 1
    let g = (fun x -> x + 1)
    let h = use ~f:(fun x -> x + 1) |}]
;;

let%expect_test "inline action handles a deeply recovered expression" =
  let source = "let opt[()\nlet claion A -x" in
  let range =
    Code_actions.range ~start_line:0 ~start_character:7 ~end_line:1 ~end_character:15
  in
  let test ?capabilities () =
    Helpers.test
      ?capabilities
      ~extra_env:[ "OCAMLRUNPARAM=l=10000" ]
      source
      (fun client ->
         let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
         let only = [ CodeActionKind.RefactorInline ] in
         let context = CodeActionContext.create ~diagnostics:[] ~only () in
         let* response =
           Fiber.collect_errors (fun () ->
             Client.request
               client
               (CodeAction (CodeActionParams.create ~textDocument ~range ~context ())))
         in
         match response with
         | Ok response ->
           Code_actions.print_code_action_result response;
           Fiber.return ()
         | Error [ { Exn_with_backtrace.exn = Jsonrpc.Response.Error.E error; _ } ] ->
           let data =
             Option.map error.data ~f:(function
               | `Assoc fields ->
                 `Assoc
                   (List.filter fields ~f:(fun (name, _) ->
                      not (String.equal name "backtrace")))
               | json -> json)
           in
           Jsonrpc.Response.Error.yojson_of_t { error with data } |> Test.print_result;
           Fiber.return ()
         | Error errors -> Fiber.reraise_all errors)
  in
  test ();
  [%expect {| No code actions |}];
  let resolveSupport = ClientCodeActionResolveOptions.create ~properties:[ "edit" ] in
  let codeAction =
    CodeActionClientCapabilities.create ~dataSupport:true ~resolveSupport ()
  in
  let textDocument = TextDocumentClientCapabilities.create ~codeAction () in
  let capabilities = ClientCapabilities.create ~textDocument () in
  test ~capabilities ();
  [%expect {| No code actions |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $x = 0 in
  x + 1
|};
  [%expect
    {|
    let _ =
      let x = 0 in
      (0) + 1 |}]
;;

let%expect_test "shadow-1" =
  inline_test
    ~print_none:true
    {|
let _ =
  let y = 1 in
  let $x = y in
  let y = 0 in
  x + 1
|};
  [%expect {| None |}]
;;

let%expect_test "shadow-2" =
  inline_test
    {|
let _ =
  let y = 1 in
  let $x y = y in
  let y = 0 in
  x y + 1
|};
  [%expect
    {|
    let _ =
      let y = 1 in
      let x y = y in
      let y = 0 in
      (y) + 1 |}]
;;

let%expect_test "shadow-3" =
  inline_test
    ~print_none:true
    {|
let _ =
  let y = 1 in
  let $x z = y + z in
  let y = 0 in
  x y + 1
|};
  [%expect {| None |}]
;;

let%expect_test "shadow-4" =
  inline_test
    ~print_none:true
    {|
module M = struct
  let y = 1
end
let _ =
  let $x = M.y in
  let module M = struct
    let y = 2
  end in
  x
|};
  [%expect {| None |}]
;;

let%expect_test "shadow-5" =
  inline_test
    {|
module M = struct
  let y = 1
end
let _ =
  let $x = M.y in
  let module N = struct
    let y = 2
  end in
  x
|};
  [%expect
    {|
    module M = struct
      let y = 1
    end
    let _ =
      let x = M.y in
      let module N = struct
        let y = 2
      end in
      (M.y) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $x = 0 + 1 in
  (fun x -> x) x
|};
  [%expect
    {|
    let _ =
      let x = 0 + 1 in
      (fun x -> x) (0 + 1) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $x = 0 + 1 in
  (fun ~x -> x) ~x
|};
  [%expect
    {|
    let _ =
      let x = 0 + 1 in
      (fun ~x -> x) ~x:(0 + 1) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $x = 0 + 1 in
  (fun ?(x = 2) -> x) ~x
|};
  [%expect
    {|
    let _ =
      let x = 0 + 1 in
      (fun ?(x = 2) -> x) ~x:(0 + 1) |}]
;;

let%expect_test "" =
  inline_test
    ~print_none:true
    {|
let _ =
  let $x = Some 0 in
  (fun ?(x = 2) -> x) ?x
|};
  [%expect {| None |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $x = 0 in
  (fun ~x -> x) ~x:(x + 1)
|};
  [%expect
    {|
    let _ =
      let x = 0 in
      (fun ~x -> x) ~x:((0) + 1) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $x = 0 in
  (fun ?(x = 1) -> x) ~x:(x + 1)
|};
  [%expect
    {|
    let _ =
      let x = 0 in
      (fun ?(x = 1) -> x) ~x:((0) + 1) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f x = x in
  f 1
|};
  [%expect
    {|
    let _ =
      let f x = x in
      (1) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f _ = 0 in
  f 1
|};
  [%expect
    {|
    let _ =
      let f _ = 0 in
      (0) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f x = x + x in
  f 1
|};
  [%expect
    {|
    let _ =
      let f x = x + x in
      (1 + 1) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f x = x + x in
  f (g 1)
|};
  [%expect
    {|
    let _ =
      let f x = x + x in
      (let x = g 1 in x + x) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f x y = x + y in
  f 0
|};
  [%expect
    {|
    let _ =
      let f x y = x + y in
      ((fun x y -> x + y) 0) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f x ~y = x + y in
  f ~y:0
|};
  [%expect
    {|
    let _ =
      let f x ~y = x + y in
      ((fun x ~y -> x + y) ~y:0) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f ~x y = x + y in
  f ~x:0
|};
  [%expect
    {|
    let _ =
      let f ~x y = x + y in
      ((fun ~x y -> x + y) ~x:0) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f ~x ~y = x + y in
  f ~y:0
|};
  [%expect
    {|
    let _ =
      let f ~x ~y = x + y in
      ((fun ~x ~y -> x + y) ~y:0) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f (x : int) = x + 1 in
  f 0
|};
  [%expect
    {|
    let _ =
      let f (x : int) = x + 1 in
      (0 + 1) |}]
;;

(* TODO: allow beta reduction with locally abstract types *)
let%expect_test "" =
  inline_test
    {|
let _ =
  let $f (type a) (x : a) = x in
  f 0
|};
  [%expect
    {|
    let _ =
      let f (type a) (x : a) = x in
      ((fun (type a) (x : a) -> x) 0) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f : int -> int = fun x -> x in
  f 0
|};
  [%expect
    {|
    let _ =
      let f : int -> int = fun x -> x in
      (0)
    |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f = function Some x -> x | None -> 0 in
  f (Some 1)
|};
  [%expect
    {|
    let _ =
      let f = function Some x -> x | None -> 0 in
      ((function | Some x -> x | None -> 0) (Some 1)) |}]
;;

(* TODO: allow beta reduction with `as` *)
let%expect_test "" =
  inline_test
    {|
let _ =
  let $f (x as y) = y + 1 in
  f 1
|};
  [%expect
    {|
    let _ =
      let f (x as y) = y + 1 in
      (let x as y = 1 in y + 1) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f 1 = 2 in
  f 2
|};
  [%expect
    {|
    let _ =
      let f 1 = 2 in
      (let 1 = 2 in 2) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f (x, y) = x + y in
  f (1, 2)
|};
  [%expect
    {|
    let _ =
      let f (x, y) = x + y in
      (1 + 2) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f (x, y) = x + y + y in
  f (1, 2 + 3)
|};
  [%expect
    {|
    let _ =
      let f (x, y) = x + y + y in
      (let y = 2 + 3 in (1 + y) + y) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f (x, y) = x + y + y in
  let z = (1, 2) in
  f z
|};
  [%expect
    {|
    let _ =
      let f (x, y) = x + y + y in
      let z = (1, 2) in
      (let (x, y) = z in (x + y) + y) |}]
;;

(* TODO *)
let%expect_test "" =
  inline_test
    {|
type t = { x : int; y : int }
let _ =
  let $f { x; y } = x + y in
  f { x = 1; y = 1 }
|};
  [%expect
    {|
    type t = { x : int; y : int }
    let _ =
      let f { x; y } = x + y in
      (let { x; y } = { x = 1; y = 1 } in x + y) |}]
;;

(* TODO: beta reduce record literals as with tuples *)
let%expect_test "" =
  inline_test
    {|
type t = { x : int; y : int }
let _ =
  let $f { x; _ } = x + 1 in
  f { x = 1; y = 1 }
|};
  [%expect
    {|
    type t = { x : int; y : int }
    let _ =
      let f { x; _ } = x + 1 in
      (let { x;_} = { x = 1; y = 1 } in x + 1) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f x = [%test] x in
  f 1
|};
  [%expect
    {|
    let _ =
      let f x = [%test] x in
      (([%test ]) 1) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f x = x in
  [%test] (f 1)
|};
  [%expect
    {|
    let _ =
      let f x = x in
      [%test] (1) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f x = (* test comment *) x in
  f 1
|};
  [%expect
    {|
    let _ =
      let f x = (* test comment *) x in
      (1) |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f x = x in
  (* test comment *) f 1
|};
  [%expect
    {|
    let _ =
      let f x = x in
      (* test comment *) (1) |}]
;;

let%expect_test "" =
  inline_test
    {|
let $f x = x
let g y = f y
|};
  [%expect
    {|
    let f x = x
    let g y = (y) |}]
;;

(* TODO *)
let%expect_test "" =
  inline_test
    {|
module M = struct
  let $f x = x
  let g y = f y
end
let h = M.f
|};
  [%expect
    {|
    module M = struct
      let f x = x
      let g y = (y)
    end
    let h = M.f |}]
;;

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f _ = 0 in
  f (print_endline "hi")
|};
  [%expect
    {|
    let _ =
      let f _ = 0 in
      (let _ = print_endline "hi" in 0) |}]
;;
