(* Representation of the typescript defined spec we're working with *)

open Import

module Literal = struct
  type t =
    | String of string
    | Int of int
    | Float of float

  let to_maybe_quoted_string = function
    | String s -> sprintf "%S" s
    | Int i -> string_of_int i
    | Float f -> string_of_float f

  let to_dyn : t -> Dyn.t =
    let open Dyn in
    function
    | String s -> string s
    | Int i -> int i
    | Float f -> float f
end

module Enum = struct
  type case =
    | Literal of Literal.t
    | Alias of string

  let dyn_of_case =
    let open Dyn in
    function
    | Literal l -> variant "Literal" [ Literal.to_dyn l ]
    | Alias l -> variant "Alias" [ string l ]

  type t = (string * case) list

  let to_dyn t =
    let open Dyn in
    list (fun (name, case) -> pair string dyn_of_case (name, case)) t
end

module type S = sig
  (** Kept abstract for resolved vs. unresolved trees *)
  type ident

  type field_def =
    | Single of
        { optional : bool
        ; typ : typ
        }
    | Pattern of
        { pat : typ
        ; typ : typ
        }

  and field = field_def Named.t

  and typ =
    | Literal of Literal.t
    | Ident of ident
    | Sum of typ list
    | List of typ
    | Record of field list
    | Tuple of typ list
    | App of typ * typ

  and interface = { fields : field list }

  and decl =
    | Interface of interface
    | Type of typ
    | Enum_anon of Enum.t

  and t = decl Named.t

  val to_dyn : t -> Dyn.t

  val dyn_of_typ : typ -> Dyn.t

  val dyn_of_field : field -> Dyn.t

  class map :
    object
      method typ : typ -> typ

      method sum : typ list -> typ

      method interface : interface -> interface

      method enum_anon : Enum.t -> Enum.t

      method field : field -> field

      method t : t -> t
    end

  class ['a] fold :
    object
      method field : field -> init:'a -> 'a

      method ident : ident -> init:'a -> 'a

      method t : t -> init:'a -> 'a

      method typ : typ -> init:'a -> 'a
    end
end

module Make (Ident : sig
  type t

  val to_dyn : t -> Dyn.t
end) =
struct
  type field_def =
    | Single of
        { optional : bool
        ; typ : typ
        }
    | Pattern of
        { pat : typ
        ; typ : typ
        }

  and field = field_def Named.t

  and typ =
    | Literal of Literal.t
    | Ident of Ident.t
    | Sum of typ list
    | List of typ
    | Record of field list
    | Tuple of typ list
    | App of typ * typ

  and interface = { fields : field list }

  and decl =
    | Interface of interface
    | Type of typ
    | Enum_anon of Enum.t

  and t = decl Named.t

  let rec dyn_of_typ =
    let open Dyn in
    function
    | Literal l -> variant "Literal" [ Literal.to_dyn l ]
    | Ident l -> variant "Ident" [ Ident.to_dyn l ]
    | Sum l -> variant "Sum" (List.map ~f:dyn_of_typ l)
    | List l -> variant "List" [ dyn_of_typ l ]
    | Tuple l -> variant "Tuple" (List.map ~f:dyn_of_typ l)
    | App (f, x) -> variant "App" [ dyn_of_typ f; dyn_of_typ x ]
    | Record fs -> variant "Record" (List.map fs ~f:dyn_of_field)

  and field_def_of_dyn =
    let open Dyn in
    function
    | Single { optional; typ } ->
      record [ ("optional", bool optional); ("typ", dyn_of_typ typ) ]
    | Pattern { pat : typ; typ : typ } ->
      record [ ("pat", dyn_of_typ pat); ("typ", dyn_of_typ typ) ]

  and dyn_of_field f = Named.to_dyn field_def_of_dyn f

  let dyn_of_interface { fields } =
    let open Dyn in
    record [ ("fields", (list dyn_of_field) fields) ]

  let dyn_of_decl =
    let open Dyn in
    function
    | Interface i -> variant "Interface" [ dyn_of_interface i ]
    | Type t -> variant "Type" [ dyn_of_typ t ]
    | Enum_anon t -> variant "Enum_anon" [ Enum.to_dyn t ]

  let to_dyn t = Named.to_dyn dyn_of_decl t

  class ['a] fold =
    object (self)
      method t (t : t) ~init =
        match t.data with
        | Interface (i : interface) ->
          List.fold_left ~init i.fields ~f:(fun init f -> self#field f ~init)
        | Type (t : typ) -> self#typ t ~init
        | Enum_anon _ -> init

      method ident _ ~init = init

      method field (f : field) ~init : 'a =
        match f.data with
        | Single { optional = _; typ } -> self#typ ~init typ
        | Pattern { pat; typ } ->
          let init = self#typ ~init pat in
          self#typ ~init typ

      method typ (t : typ) ~init =
        match t with
        | Literal _ -> init
        | Ident i -> self#ident i ~init
        | App (t1, t2) ->
          let init = self#typ t1 ~init in
          self#typ t2 ~init
        | List t -> self#typ t ~init
        | Tuple typs | Sum typs ->
          List.fold_left typs ~init ~f:(fun init f -> self#typ f ~init)
        | Record fs ->
          List.fold_left fs ~init ~f:(fun init f -> self#field f ~init)
    end

  class map =
    object (self)
      method field (f : field) =
        let data =
          match f.data with
          | Single s ->
            let typ = self#typ s.typ in
            Single { s with typ }
          | Pattern { pat; typ } ->
            let pat = self#typ pat in
            let typ = self#typ typ in
            Pattern { pat; typ }
        in
        { f with data }

      method interface (i : interface) =
        let fields = List.map ~f:self#field i.fields in
        { fields }

      method sum (constrs : typ list) = Sum (List.map constrs ~f:self#typ)

      method typ (t : typ) =
        match t with
        | Literal i -> Literal i
        | Ident i -> Ident i
        | App (x, y) ->
          let x = self#typ x
          and y = self#typ y in
          App (x, y)
        | List t -> List (self#typ t)
        | Tuple ts -> Tuple (List.map ts ~f:self#typ)
        | Sum ts -> self#sum ts
        | Record ts -> Record (List.map ts ~f:self#field)

      method enum_anon (t : Enum.t) = t

      method t (t : t) =
        let data =
          match t.data with
          | Interface i -> Interface (self#interface i)
          | Type t -> Type (self#typ t)
          | Enum_anon t -> Enum_anon (self#enum_anon t)
        in
        { t with data }
    end
end

module Unresolved = struct
  (** In the unresolved AST, all identifiers are just strings *)
  include Make (String)

  let enum ~name ~constrs : Enum.t Named.t = { Named.name; data = constrs }

  let interface ~name ~fields : interface Named.t =
    { Named.name; data = { fields } }

  let pattern_field ~name ~pat ~typ =
    { Named.name; data = Pattern { pat; typ } }

  let named_field ?(optional = false) typ name =
    { Named.name; data = Single { optional; typ } }
end

module Ident = struct
  module Id = Stdune.Id.Make ()

  module T = struct
    type t =
      { id : Id.t
      ; name : string
      }

    let to_dyn { id; name } =
      let open Dyn in
      record [ ("id", Id.to_dyn id); ("name", String name) ]

    let compare t { id; name = _ } = Id.compare t.id id
  end

  include T

  let make name = { name; id = Id.gen () }

  module C = Comparable.Make (T)
  module Set = C.Set
  module Top_closure = Top_closure.Make (Set) (Stdune.Monad.Id)
end

module Prim = struct
  type t =
    | Null
    | String
    | Bool
    | Number
    | Uinteger
    | Any
    | Object
    | List
    | Self
    | Resolved of Ident.t

  let to_dyn =
    let open Dyn in
    function
    | Null -> variant "Null" []
    | String -> variant "String" []
    | Bool -> variant "Bool" []
    | Number -> variant "Number" []
    | Uinteger -> variant "Uinteger" []
    | Any -> variant "Any" []
    | Object -> variant "Object" []
    | List -> variant "List" []
    | Self -> variant "Self" []
    | Resolved r -> variant "Resolved" [ Ident.to_dyn r ]

  let of_string s ~resolve =
    match String.lowercase_ascii s with
    | "null" -> Null
    | "string" -> String
    | "boolean" -> Bool
    | "number" -> Number
    | "uinteger" -> Uinteger
    | "json" -> Any
    | "lspany" -> Any
    | "array" -> List
    | "object" -> Object
    | "lspobject" -> Object
    | _ -> resolve s
end

module Resolved = Make (Prim)

let subst unresolved =
  object
    val params = String.Map.empty

    val inside = None

    (* Resolve self references. *)
    method inside s = {<inside = Some s>}

    method resolve n =
      match String.Map.find params n with
      | Some [] -> assert false
      | Some (x :: _) -> `Resolved x
      | None ->
        if inside = Some n then `Self
        else `Unresolved (String.Map.find_exn unresolved n)

    method push x y =
      let params =
        String.Map.update params x ~f:(function
            | None -> Some [ y ]
            | Some [] -> assert false
            | Some (y' :: xs) -> if y = y' then Some xs else Some (y :: y' :: xs))
      in
      {<params>}

    method pop x =
      let params =
        String.Map.update params x ~f:(function
            | None ->
              ignore (String.Map.find_exn params x);
              None
            | Some [] -> assert false
            | Some (_ :: xs) -> Some xs)
      in
      {<params>}
  end

let rec resolve_all ts ~(names : Ident.t String.Map.t) : Resolved.t list =
  let names = subst names in
  List.map ts ~f:(resolve ~names)

and resolve (t : Unresolved.t) ~names : Resolved.t =
  let data : Resolved.decl =
    match t.data with
    | Interface i -> Interface (resolve_interface { t with data = i } ~names)
    | Type t -> Type (resolve_type t ~names)
    | Enum_anon a -> Enum_anon a
  in
  { t with Named.data }

and resolve_ident i ~names : Prim.t =
  Prim.of_string i ~resolve:(fun s ->
      match names#resolve s with
      | `Resolved s -> s
      | `Self -> Self
      | `Unresolved s -> Resolved s)

and resolve_type (t : Unresolved.typ) ~names : Resolved.typ =
  match t with
  | Literal l -> Literal l
  | Ident i -> Ident (resolve_ident ~names i)
  | Sum l -> Sum (List.map ~f:(resolve_type ~names) l)
  | Tuple l -> Tuple (List.map ~f:(resolve_type ~names) l)
  | App (f, x) -> App (resolve_type ~names f, resolve_type ~names x)
  | List t -> List (resolve_type t ~names)
  | Record fields -> Record (List.map ~f:(resolve_field ~names) fields)

and resolve_interface i ~names : Resolved.interface =
  let names = names#inside i.name in
  let i = i.data in
  { fields = List.map ~f:(resolve_field ~names) i.fields }

and resolve_field f ~names : Resolved.field =
  let data : Resolved.field_def =
    match f.data with
    | Single { optional; typ } ->
      let typ = resolve_type ~names typ in
      Single { optional; typ }
    | Pattern { pat; typ } ->
      let typ = resolve_type ~names typ in
      let pat = resolve_type ~names pat in
      Pattern { pat; typ }
  in
  { f with Named.data }
