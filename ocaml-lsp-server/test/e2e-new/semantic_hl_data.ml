let src0 =
  {|
module Moo : sig
  type t

  type koo =
    | Foo of string
    | Bar of [ `Int of int | `String of string ]

  val u : unit

  val f : unit -> t
end = struct
  type t = int

  type koo =
    | Foo of string
    | Bar of [ `Int of int | `String of string ]

  let u = ()

  let f () = 0
end

module type Bar = sig
  type t =
    { foo : Moo.t
    ; bar : int
    }
end

type t = Moo.koo =
  | Foo of string
  | Bar of [ `BarInt of int | `BarString of string ]

let f (foo : t) =
  match foo with
  | Moo.Foo s -> s ^ string_of_int 0
  | Moo.Bar (`BarInt i) -> string_of_int i
  | Moo.Bar (`BarString s) -> s

module Foo (Arg : Bar) = struct
  module Inner_foo = struct
    type t = string
  end
end

module Foo_inst = Foo (struct
  type t =
    { foo : Moo.t
    ; bar : int
    }
end)
|}
;;
