let wrap_test = Code_actions.code_action_test ~title:"Wrap-type-in-module"

let%expect_test "preserving whitespace" =
  wrap_test "t$y$pe foo = bar";
  [%expect
    {|
    module Foo = struct
      type t = bar
    end
    |}];
  wrap_test "t$y$pe foo= bar";
  [%expect
    {|
    module Foo = struct
      type t= bar
    end
    |}];
  wrap_test "t$y$pe foo\n=\nbar";
  [%expect
    {|
    module Foo = struct
      type t
      =
      bar
    end
    |}];
  wrap_test
      (
    String.concat
      "\n"
      [ "typ$e$ ('a, 'b) a = { bar : 'a";
        "                  ; baz : 'b";
        "                  }" ]
      [@ocamlformat "disable"]);
  [%expect
    {|
    module A = struct
      type ('a, 'b) t = { bar : 'a
                        ; baz : 'b
                        }
    end
    |}];
  (* non-space character can come before the type name *)
  wrap_test
    (String.concat
      "\n"
      [ "ty$p$e";
        "abc = { a: int;";
        "b: int; c: int } [@@deriving sexp]" ]) [@ocamlformat "disable"];
  [%expect
    {|
    module Abc = struct
      type
      t = { a: int;
      b: int; c: int } [@@deriving sexp]
    end
    |}];
  (* entire module is indented by correct amount *)
  wrap_test
    (String.concat
        "\n"
        [ "module Outer = struct"
        ; "  module Inner = struct"
        ; "    type record ="
        ; "      { foo : int"
        ; "      ; bar : in$t$"
        ; "      }"
        ; "  end"
        ; "end"
        ]);
  [%expect
    {|
    module Outer = struct
      module Inner = struct
        module Record = struct
          type t =
            { foo : int
            ; bar : int
            }
        end
      end
    end
    |}];
;;

let%expect_test "type parameters" =
  wrap_test "t$y$pe ('a, 'b, _) foo = bar";
  [%expect
    {|
    module Foo = struct
      type ('a, 'b, _) t = bar
    end
    |}];
;;

let%expect_test "definition chain" =
    wrap_test
      {xxx|module Module = struct
  module Foo = struct
    type ('a, 'b) t =
      { a : 'a
      ; b : 'b
      }
  end
end

ty$p$e ('a, 'b) foo = ('a, 'b) Module.Foo.t =
  { a : 'a
  ; b : 'b
  }
|xxx};
  [%expect
    {|
    module Module = struct
      module Foo = struct
        type ('a, 'b) t =
          { a : 'a
          ; b : 'b
          }
      end
    end

    module Foo = struct
      type ('a, 'b) t = ('a, 'b) Module.Foo.t =
        { a : 'a
        ; b : 'b
        }
    end
    |}];
;;

let%expect_test "can trigger action on any part of type declaration" =
  wrap_test {|type abc = { a: int; b: int; c: int } [@@derivin$g$ sexp]|};
  [%expect
    {|
    module Abc = struct
      type t = { a: int; b: int; c: int } [@@deriving sexp]
    end
    |}];
  wrap_test {|type abc = { a: int; b: int;$ $c: int } [@@deriving sexp]|};
  [%expect
    {|
    module Abc = struct
      type t = { a: int; b: int; c: int } [@@deriving sexp]
    end
    |}];
  wrap_test {|type a$b$c = { a: int; b: int; c: int } [@@deriving sexp]|};
  [%expect
    {|
    module Abc = struct
      type t = { a: int; b: int; c: int } [@@deriving sexp]
    end
    |}];
  wrap_test {|typ$e$ abc = { a: int; b: int; c: int } [@@deriving sexp]|};
  [%expect
    {|
    module Abc = struct
      type t = { a: int; b: int; c: int } [@@deriving sexp]
    end
    |}];
;;

let%expect_test "type with name t is ignored" =
  wrap_test {|type $t$ = int|};
  [%expect {| |}]
;;

let%expect_test "produce sig in mli" =
  wrap_test
    ?path:(Some "needs-refactoring.mli")
    {|type abc = { a: int; b: int; c: int } [@@derivin$g$ sexp]|};
  [%expect
    {|
    module Abc : sig
      type t = { a: int; b: int; c: int } [@@deriving sexp]
    end
    |}]
;;
