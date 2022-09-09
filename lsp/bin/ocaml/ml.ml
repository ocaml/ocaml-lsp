open Import

module Kind = struct
  type t =
    | Intf
    | Impl

  type ('intf, 'impl) pair =
    { intf : 'intf
    ; impl : 'impl
    }

  module Map = struct
    type 'a t = ('a, 'a) pair

    let get { intf; impl } = function
      | Impl -> impl
      | Intf -> intf

    let make_both a = { intf = a; impl = a }

    let iter { intf; impl } ~f =
      f intf;
      f impl

    let map { intf; impl } ~f = { intf = f intf; impl = f impl }

    let both (type a b) (x : a t) (y : b t) : (a * b) t =
      { intf = (x.intf, y.intf); impl = (x.impl, y.impl) }
  end
end

let is_kw = function
  | "type" | "method" | "end" | "to" | "external" -> true
  | _ -> false

module Arg = struct
  type 'e t =
    | Unnamed of 'e
    | Labeled of string * 'e
    | Optional of string * 'e

  let to_dyn f =
    let open Dyn in
    function
    | Unnamed a -> Dyn.variant "Unnamed" [ f a ]
    | Labeled (s, a) -> Dyn.variant "Labeled" [ string s; f a ]
    | Optional (s, a) -> Dyn.variant "Optional" [ string s; f a ]
end

module Path = struct
  type t =
    | Ident of string
    | Dot of t * string
    | Apply of t * t

  let rec to_string = function
    | Ident s -> s
    | Dot (t, s) -> to_string t ^ "." ^ s
    | Apply (f, x) -> to_string f ^ "(" ^ to_string x ^ ")"

  let rec pp = function
    | Ident s -> Pp.verbatim s
    | Dot (s, p) -> Pp.concat [ pp s; Pp.verbatim "."; Pp.verbatim p ]
    | Apply (s, p) -> Pp.concat [ pp s; W.surround `Paren (pp p) ]
end

module Type = struct
  [@@@warning "-30"]

  type prim =
    | Unit
    | String
    | Int
    | Bool

  let dyn_of_prim : prim -> Dyn.t =
    let open Dyn in
    function
    | Unit -> variant "Unit" []
    | String -> variant "String" []
    | Int -> variant "Int" []
    | Bool -> variant "Bool" []

  type t =
    | Path of Path.t
    | Var of string
    | Prim of prim
    | Tuple of t list
    | Optional of t
    | List of t
    | Poly_variant of constr list
    | Assoc of t * t
    | App of t * t list
    | Fun of t Arg.t * t

  and constr =
    { name : string
    ; args : t list
    }

  and field =
    { name : string
    ; typ : t
    ; attrs : (string * string list) list
    }

  let rec to_dyn =
    let open Dyn in
    function
    | Var v -> variant "Var" [ string v ]
    | List v -> variant "List" [ to_dyn v ]
    | Assoc (x, y) -> variant "Assoc" [ to_dyn x; to_dyn y ]
    | Tuple xs -> variant "Tuple" (List.map ~f:to_dyn xs)
    | Optional t -> variant "Optional" [ to_dyn t ]
    | Path p -> variant "Path" [ string @@ Path.to_string p ]
    | Poly_variant xs -> variant "Poly_variant" (List.map ~f:dyn_of_constr xs)
    | App (x, y) -> variant "App" (to_dyn x :: List.map y ~f:to_dyn)
    | Prim p -> variant "Prim" [ dyn_of_prim p ]
    | Fun (arg, t) -> variant "Fun" [ Arg.to_dyn to_dyn arg; to_dyn t ]

  and dyn_of_constr { name; args } =
    Dyn.(record [ ("name", string name); ("args", (list to_dyn) args) ])

  and dyn_of_field { name; typ; attrs } =
    let open Dyn in
    record
      [ ("name", string name)
      ; ("typ", to_dyn typ)
      ; ("attrs", list (pair string (list string)) attrs)
      ]

  type decl =
    | Alias of t
    | Record of field list
    | Variant of constr list

  let dyn_of_decl =
    let open Dyn in
    function
    | Alias a -> variant "Alias" [ to_dyn a ]
    | Record fs -> variant "Record" (List.map ~f:dyn_of_field fs)
    | Variant cs -> variant "Variant" (List.map ~f:dyn_of_constr cs)

  class virtual ['env, 'm] mapreduce =
    object (self : 'self)
      method virtual empty : 'm

      method virtual plus : 'm -> 'm -> 'm

      method poly_variant env constrs =
        let r, s = self#fold_left_map constrs ~f:(fun c -> self#constr env c) in
        (Poly_variant r, s)

      method tuple (env : 'env) t =
        let (r : t list), s =
          self#fold_left_map t ~f:(fun (t : t) -> self#t env t)
        in
        (Tuple r, s)

      method path _ p = (Path p, self#empty)

      method var _ n = (Var n, self#empty)

      method prim _ p = (Prim p, self#empty)

      method optional env p =
        let t, s = self#t env p in
        (Optional t, s)

      method list env t =
        let t, s = self#t env t in
        (List t, s)

      method assoc env k v =
        let k, s1 = self#t env k in
        let v, s2 = self#t env v in
        (Assoc (k, v), self#plus s1 s2)

      method app env f xs =
        let f, s1 = self#t env f in
        let xs, s2 = self#fold_left_map xs ~f:(fun x -> self#t env x) in
        (App (f, xs), self#plus s1 s2)

      method t env this =
        match (this : t) with
        | Path p -> self#path env p
        | Var v -> self#var env v
        | Prim p -> self#prim env p
        | Tuple t -> self#tuple env t
        | Optional t -> self#optional env t
        | List t -> self#list env t
        | Poly_variant t -> self#poly_variant env t
        | Assoc (k, v) -> self#assoc env k v
        | App (f, xs) -> self#app env f xs
        | Fun (_, _) -> assert false

      method alias env t =
        let r0, s0 = self#t env t in
        (Alias r0, s0)

      method constr env (constr : constr) =
        let args, s =
          self#fold_left_map constr.args ~f:(fun t -> self#t env t)
        in
        ({ constr with args }, s)

      method private fold_left_map
          : 'a. f:('a -> 'a * 'm) -> 'a list -> 'a list * 'm =
        fun ~f xs ->
          let accf, accm =
            List.fold_left xs ~init:([], self#empty) ~f:(fun (accf, accm) x ->
                let r, s = f x in
                (r :: accf, self#plus accm s))
          in
          (List.rev accf, accm)

      method field env f =
        let typ, s = self#t env f.typ in
        ({ f with typ }, s)

      method record env fields =
        let r, s = self#fold_left_map fields ~f:(fun f -> self#field env f) in
        (Record r, s)

      method variant env constrs =
        let v, s = self#fold_left_map constrs ~f:(fun f -> self#constr env f) in
        (Variant v, s)

      method decl env decl =
        match decl with
        | Alias a -> self#alias env a
        | Record fs -> self#record env fs
        | Variant v -> self#variant env v
    end

  let field typ ~name = { name; typ; attrs = [] }

  let fun_ args t =
    List.fold_right args ~init:t ~f:(fun arg acc -> Fun (arg, acc))

  let constr args ~name = { name; args }

  let list t = List t

  let assoc_list ~key ~data = Assoc (key, data)

  let t = Path (Ident "t")

  let module_t m = Path (Dot (Ident (String.capitalize_ascii m), "t"))

  let string = Prim String

  let name s = Path (Ident s)

  let int = Prim Int

  let bool = Prim Bool

  let alpha = Var "a"

  let enum constrs =
    Variant (List.map constrs ~f:(fun constr -> { name = constr; args = [] }))

  let poly_enum constrs =
    Poly_variant
      (List.map constrs ~f:(fun constr -> { name = constr; args = [] }))

  let json = Path (Dot (Ident "Json", "t"))

  let unit = Prim Unit

  let array t = App (Path (Ident "array"), [ t ])

  let void =
    let void = Path.Dot (Ident "Json", "Void") in
    Path (Dot (void, "t"))

  let json_object =
    let obj = Path.Dot (Ident "Json", "Object") in
    Path (Dot (obj, "t"))

  module Type = W.Type

  let pp_prim (p : prim) : W.t =
    match p with
    | String -> Pp.verbatim "string"
    | Int -> Pp.verbatim "int"
    | Bool -> Pp.verbatim "bool"
    | Unit -> Pp.verbatim "unit"

  let rec pp (a : t) ~(kind : Kind.t) : W.t =
    match a with
    | Prim p -> pp_prim p
    | Var v -> Type.var v
    | Path p -> Path.pp p
    | App (f, xs) -> Type.app (pp ~kind f) (List.map ~f:(pp ~kind) xs)
    | Tuple t -> Type.tuple (List.map ~f:(pp ~kind) t)
    | Optional t -> pp ~kind (App (Path (Ident "option"), [ t ]))
    | List t -> pp ~kind (App (Path (Ident "list"), [ t ]))
    | Poly_variant constrs ->
      List.map constrs ~f:(fun { name; args } ->
          (name, List.map args ~f:(pp ~kind)))
      |> Type.poly
    | Assoc (k, v) ->
      let t = List (Tuple [ k; v ]) in
      pp t ~kind
    | Fun (a, r) -> (
      match a with
      | Arg.Unnamed t ->
        Pp.concat
          [ pp t ~kind; Pp.space; Pp.verbatim "->"; Pp.space; pp ~kind r ]
      | Arg.Labeled (l, t) ->
        Pp.concat
          [ Pp.textf "%s:" l
          ; pp t ~kind
          ; Pp.space
          ; Pp.verbatim "->"
          ; Pp.space
          ; pp ~kind r
          ]
      | Arg.Optional (l, t) ->
        Pp.concat
          [ Pp.textf "?%s:" l
          ; pp t ~kind
          ; Pp.space
          ; Pp.verbatim "->"
          ; Pp.space
          ; pp ~kind r
          ])

  let pp_decl' ~(kind : Kind.t) (a : decl) =
    match a with
    | Alias a -> (
      let pp = pp ~kind a in
      match (a, kind) with
      | (List _ | Path _ | Prim _), Impl -> W.Type.deriving ~record:false pp
      | _, _ -> pp)
    | Variant v ->
      List.map v ~f:(fun { name; args } -> (name, List.map ~f:(pp ~kind) args))
      |> Type.variant
    | Record r -> (
      let r =
        List.map r ~f:(fun { name; typ; attrs } ->
            let def =
              let field = pp ~kind typ in
              let attrs =
                let attrs =
                  match kind with
                  | Intf -> []
                  | Impl -> attrs
                in
                List.concat_map attrs ~f:(fun (a, r) ->
                    [ W.Attr.make a (List.map ~f:Pp.verbatim r) ])
              in
              Type.field_attrs ~field ~attrs
            in
            (name, def))
        |> Type.record
      in
      match kind with
      | Intf -> r
      | Impl -> W.Type.deriving r ~record:true)

  let pp_decl ~name ~kind (a : decl) : W.t =
    let body = pp_decl' ~kind a in
    Type.decl name body
end

module Expr = struct
  [@@@ocaml.warning "-30-32-37"]

  type expr =
    | Let of pat * expr * expr  (** let pat = e1 in e2 *)
    | Match of expr * (pat * expr) list  (** match e1 with [p -> e]* *)
    | Fun of pat Arg.t list * expr  (** fun p2 p2 .. -> e *)
    | App of expr * expr Arg.t list  (** f e1 e2 .. *)
    | Create of expr prim  (** Literal/Primitive *)
    | Assert_false  (** assert false *)

  and 'e prim =
    | Unit
    | Bool of bool
    | Int of int
    | String of string
    | Ident of string
    | Cons of 'e * 'e prim
    | List of 'e list
    | Tuple of 'e list
    | Record of 'e record_
    | Constr of 'e constr

  and pat =
    | Wildcard
    | Pat of pat prim

  and 'e record_ = (string * 'e) list

  and 'e constr =
    { tag : string
    ; poly : bool
    ; args : 'e list
    }

  type t = expr

  let assert_false_clause = (Wildcard, Assert_false)

  type toplevel =
    { pat : (string Arg.t * Type.t) list
    ; type_ : Type.t
    ; body : t
    }

  let constr ?(poly = false) ?(args = []) tag = { poly; args; tag }

  let pp_constr f { tag; poly; args } =
    let tag =
      let tag = String.capitalize tag in
      Pp.verbatim (if poly then "`" ^ tag else tag)
    in
    match args with
    | [] -> tag
    | args ->
      let sep = Pp.verbatim "," in
      let args = W.surround `Paren (Pp.concat_map ~sep ~f args) in
      Pp.concat [ tag; Pp.space; args ]

  let rec pp_pat = function
    | Wildcard -> Pp.verbatim "_"
    | Pat pat -> (
      match pat with
      | Unit -> Pp.verbatim "()"
      | Bool b -> Pp.textf "%b" b
      | Int i -> Pp.textf "%i" i
      | String s -> Pp.textf "%S" s
      | Ident s -> Pp.verbatim s
      | Cons _ -> assert false
      | List _ -> assert false
      | Tuple _ -> assert false
      | Record _ -> assert false
      | Constr c -> pp_constr pp_pat c)

  let rec pp_create : expr prim -> _ Pp.t = function
    | Unit -> Pp.verbatim "()"
    | Bool b -> Pp.textf "%b" b
    | Int i ->
      let pp = Pp.textf "%i" i in
      if i < 0 then W.surround `Paren pp else pp
    | String s -> Pp.textf "%S" s
    | Ident s -> Pp.verbatim s
    | Cons _ -> assert false
    | List xs ->
      let xs = Pp.concat_map xs ~sep:(Pp.verbatim ";") ~f:pp in
      W.surround `Square xs
    | Tuple _ -> assert false
    | Record fields ->
      let record =
        let open Pp.O in
        Pp.concat_map
          fields
          ~sep:(Pp.verbatim ";" ++ Pp.space)
          ~f:(fun (name, expr) ->
            if expr = Create (Ident name) then pp expr
            else Pp.verbatim name ++ Pp.space ++ Pp.verbatim "=" ++ pp expr)
      in
      W.surround `Curly record
    | Constr c -> pp_constr pp c

  and pp = function
    | Assert_false -> Pp.verbatim "assert false"
    | Match (expr, patterns) ->
      let with_ =
        Pp.concat
          [ Pp.verbatim "match"
          ; Pp.space
          ; pp expr
          ; Pp.space
          ; Pp.verbatim "with"
          ]
      in
      let clauses =
        Pp.concat_map patterns ~f:(fun (pat, expr) ->
            Pp.concat
              [ Pp.verbatim "| "
              ; pp_pat pat
              ; Pp.space
              ; Pp.verbatim "->"
              ; Pp.space
              ; Pp.verbatim "("
              ; pp expr
              ; Pp.verbatim ")"
              ])
      in
      Pp.concat [ with_; Pp.newline; clauses ]
    | Create c -> pp_create c
    | App (x, args) ->
      let args =
        Pp.concat_map args ~sep:Pp.space ~f:(fun arg ->
            match arg with
            | Unnamed e -> pp e
            | _ -> assert false)
      in
      Pp.concat [ pp x; Pp.space; args ]
    | Fun (pats, expr) ->
      W.surround
        `Paren
        (Pp.concat
           [ Pp.verbatim "fun"
           ; Pp.space
           ; Pp.concat_map pats ~sep:Pp.space ~f:(fun arg ->
                 match arg with
                 | Unnamed e -> pp_pat e
                 | _ -> assert false)
           ; Pp.space
           ; Pp.verbatim "->"
           ; pp expr
           ])
    | _ -> assert false

  let pp_toplevel ~kind name { pat; type_; body } =
    let pat =
      Pp.concat_map pat ~f:(fun (pat, typ) ->
          let typ = Type.pp ~kind typ in
          match pat with
          | Unnamed s ->
            Pp.concat
              [ Pp.verbatim "("
              ; Pp.verbatim s
              ; Pp.verbatim " : "
              ; typ
              ; Pp.verbatim ")"
              ]
          | Labeled (l, r) ->
            if l = r then
              Pp.concat [ Pp.textf "~(%s :" l; typ; Pp.verbatim ")" ]
            else assert false
          | Optional (l, r) ->
            if l = r then
              Pp.concat
                [ Pp.textf "?(%s :" l; typ; Pp.space; Pp.verbatim "option)" ]
            else assert false)
    in
    let body = pp body in
    let type_ = Type.pp type_ ~kind in
    Pp.concat
      [ Pp.textf "let %s" name
      ; pat
      ; Pp.textf " : "
      ; type_
      ; Pp.textf "="
      ; Pp.newline
      ; body
      ]
end

module Module = struct
  module Name : sig
    type t = private string

    val of_string : string -> t
  end = struct
    type t = string

    let of_string s =
      match s.[0] with
      | 'a' .. 'z' ->
        Code_error.raise "invalid module name" [ ("s", Dyn.string s) ]
      | _ -> s
  end

  type 'a t =
    { name : Name.t
    ; bindings : 'a Named.t list
    }

  let empty name = { name; bindings = [] }

  type sig_ =
    | Value of Type.t
    | Type_decl of Type.decl
    | Include of Name.t * (Type.t * Type.t) list

  type impl =
    | Type_decl of Type.decl
    | Value of Expr.toplevel

  let pp_sig { name; bindings } =
    let bindings =
      Pp.concat_map bindings ~sep:Pp.newline ~f:(fun { name; data } ->
          match (data : sig_) with
          | Value t -> W.Sig.val_ name [ Type.pp ~kind:Intf t ]
          | Type_decl t -> W.Type.decl name (Type.pp_decl' ~kind:Intf t)
          | Include (mod_, destructive_subs) ->
            List.map destructive_subs ~f:(fun (l, r) ->
                let f = Type.pp ~kind:Intf in
                (f l, f r))
            |> W.Sig.include_ (mod_ :> string))
    in
    W.Sig.module_ (name :> string) bindings

  let pp_impl { name; bindings } =
    let bindings =
      Pp.concat_map bindings ~sep:Pp.newline ~f:(fun { name; data = v } ->
          match v with
          | Value decl -> Expr.pp_toplevel ~kind:Impl name decl
          | Type_decl t -> W.Type.decl name (Type.pp_decl' ~kind:Impl t))
    in
    W.module_ (name :> string) bindings
end
