module Stdune_table = Table

module type S = sig
  type t

  val hash : t -> int

  val equal : t -> t -> bool

  val compare : t -> t -> Ordering.t

  val to_dyn : t -> Dyn.t

  val to_string : t -> string

  val pp : t Fmt.t

  val make : string -> t

  val get : string -> t option

  val all : unit -> t list

  module Set : sig
    include Set.S with type elt = t

    val to_dyn : t -> Dyn.t

    val make : string list -> t

    val pp : t Fmt.t
  end

  module Map : Map.S with type key = t

  module Table : sig
    type key = t

    type 'a t

    val create : default_value:'a -> 'a t

    val get : 'a t -> key -> 'a

    val set : 'a t -> key:key -> data:'a -> unit
  end
  with type key := t
end

type resize_policy =
  | Conservative
  | Greedy

type order =
  | Natural
  | Fast

let new_size ~next ~size = function
  | Conservative ->
    let increment_size = 512 in
    (next land lnot (increment_size - 1)) + (increment_size * 2)
  | Greedy -> size * 2

module type Settings = sig
  val initial_size : int

  val resize_policy : resize_policy

  val order : order
end

module Make (R : Settings) () = struct
  let ids = Table.create (module String) 1024

  let next = ref 0

  module Table = struct
    type 'a t =
      { default_value : 'a
      ; mutable data : 'a array
      }

    let create ~default_value =
      { default_value; data = Array.make R.initial_size default_value }

    let resize t =
      let n =
        new_size ~next:!next ~size:(Array.length t.data) R.resize_policy
      in
      let old_data = t.data in
      let new_data = Array.make n t.default_value in
      t.data <- new_data;
      Array.blit ~src:old_data ~src_pos:0 ~dst:new_data ~dst_pos:0
        ~len:(Array.length old_data)

    let get t key =
      if key >= Array.length t.data then
        t.default_value
      else
        t.data.(key)

    let set t ~key ~data =
      if key >= Array.length t.data then resize t;
      t.data.(key) <- data
  end

  let names = Table.create ~default_value:""

  let make s =
    Stdune_table.find_or_add ids s ~f:(fun s ->
        let n = !next in
        next := n + 1;
        Table.set names ~key:n ~data:s;
        n)

  let get s = Stdune_table.find ids s

  let to_string t = Table.get names t

  let hash t = String.hash (to_string t)

  let all () = List.init !next ~f:(fun t -> t)

  module T = struct
    type nonrec t = int

    let compare =
      match R.order with
      | Fast -> Int.compare
      | Natural -> fun x y -> String.compare (to_string x) (to_string y)

    let equal x y = compare x y = Ordering.Eq

    let to_dyn = Dyn.Encoder.int
  end

  include T

  let pp fmt t = Format.fprintf fmt "%S" (to_string t)

  module O = Comparable.Make (T)

  module Set = struct
    include O.Set

    let make l = List.fold_left l ~init:empty ~f:(fun acc s -> add acc (make s))

    let pp fmt (t : t) = Fmt.ocaml_list pp fmt (to_list t)
  end

  module Map = Map.Make (T)
end

module No_interning (R : Settings) () = struct
  type t = string

  let compare = String.compare

  let hash = String.hash

  let equal = String.equal

  let make s = s

  let to_string s = s

  let pp fmt s = Format.fprintf fmt "%S" (to_string s)

  let get s = Some s

  let all () = assert false

  let to_dyn t = Dyn.String (to_string t)

  module Set = struct
    include String.Set

    let make = of_list

    let pp fmt t = Fmt.ocaml_list Format.pp_print_string fmt (to_list t)
  end

  module Map = String.Map

  module Table = struct
    type 'a t =
      { default_value : 'a
      ; data : (string, 'a) Stdune_table.t
      }

    let create ~default_value =
      { default_value
      ; data = Stdune_table.create (module String) R.initial_size
      }

    let get t k =
      match Stdune_table.find t.data k with
      | None -> t.default_value
      | Some s -> s

    let set t ~key ~data = Stdune_table.set t.data key data
  end
end
