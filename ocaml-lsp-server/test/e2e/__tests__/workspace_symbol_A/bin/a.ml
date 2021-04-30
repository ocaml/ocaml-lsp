let a_x = 5

module A_B = struct
  type a_b_t = string
  let a_b = "hello"
end

let a_d =
  match "" with
  | "" -> true
  | _ -> false

type user =
  | Admin
  | NotAdmin

let a_u = Admin

let a_arr = []

let a_m, a_n = (1, 2)

let a_i =
  let a_i_h = 6 in
  a_i_h

module StringMap = Map.Make (String)
module My_string = String

module A_Mod : sig
  type t = int

  val compare : t -> t -> int
end = struct
  type t = int

  let private_mod_fn = Stdlib.abs

  let compare = Stdlib.compare
end

module type X_int = sig
  val x : int
end

module Increment (M : X_int) = struct
  let increment_x = M.x + 1
end

exception Foo of string

class stack_of_ints =
  object
    val mutable the_list : int list = []

    method push x = the_list <- x :: the_list

    method pop =
      let result = List.hd the_list in
      the_list <- List.tl the_list;
      result

    method peek = List.hd the_list

    method size = List.length the_list
  end
