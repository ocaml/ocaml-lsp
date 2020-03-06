module Var = struct
  module T = struct
    type t = string

    let compare =
      if Sys.win32 then
        fun a b ->
      String.compare (String.lowercase a) (String.lowercase b)
      else
        String.compare

    let to_dyn = Dyn.Encoder.string
  end

  include Comparable.Make (T)
  include T
end

module Map = Map.Make (Var)

type t =
  { vars : string Map.t
  ; mutable unix : string array option
  }

let equal t { vars; unix = _ } = Map.equal ~equal:String.equal t.vars vars

let hash { vars; unix = _ } = Hashtbl.hash vars

let make vars = { vars; unix = None }

let empty = make Map.empty

let vars t = Var.Set.of_list (Map.keys t.vars)

let get t k = Map.find t.vars k

let to_unix t =
  match t.unix with
  | Some v -> v
  | None ->
    let res =
      Map.foldi ~init:[]
        ~f:(fun k v acc -> Printf.sprintf "%s=%s" k v :: acc)
        t.vars
      |> Array.of_list
    in
    t.unix <- Some res;
    res

let of_unix arr =
  Array.to_list arr
  |> List.map ~f:(fun s ->
         match String.lsplit2 s ~on:'=' with
         | None ->
           Code_error.raise
             "Env.of_unix: entry without '=' found in the environ"
             [ ("var", String s) ]
         | Some (k, v) -> (k, v))
  |> Map.of_list_multi
  |> Map.map ~f:(function
       | [] -> assert false
       | x :: _ -> x)

let initial = make (of_unix (Unix.environment ()))

let add t ~var ~value = make (Map.set t.vars var value)

let remove t ~var = make (Map.remove t.vars var)

let extend t ~vars = make (Map.union t.vars vars ~f:(fun _ _ v -> Some v))

let extend_env x y = extend x ~vars:y.vars

let to_dyn t =
  let open Dyn.Encoder in
  Map.to_dyn string t.vars

let diff x y =
  Map.merge x.vars y.vars ~f:(fun _k vx vy ->
      match vy with
      | Some _ -> None
      | None -> vx)
  |> make

let update t ~var ~f = make (Map.update t.vars var ~f)

let of_string_map m =
  make (String.Map.foldi ~init:Map.empty ~f:(fun k v acc -> Map.set acc k v) m)

let iter t = Map.iteri t.vars

let cons_path t ~dir =
  make
    (Map.update t.vars "PATH" ~f:(fun _PATH -> Some (Bin.cons_path dir ~_PATH)))

let path env =
  match get env "PATH" with
  | None -> []
  | Some s -> Bin.parse_path s
