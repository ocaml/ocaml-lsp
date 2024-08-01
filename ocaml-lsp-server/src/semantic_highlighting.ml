open Import
open Fiber.O
module Array_view = Lsp.Private.Array_view

(* TODO:

   - [x] support textDocument/semanticTokens/full

   - [x] support textDocument/semanticTokens/full/delta

   - [ ] support textDocument/semanticTokens/range *)

module Token_type : sig
  type t

  val of_builtin : SemanticTokenTypes.t -> t
  val module_ : t
  val module_type : t
  val to_int : t -> int
  val to_legend : t -> string
  val tokenTypes : string list
end = struct
  type t = SemanticTokenTypes.t

  let of_builtin t = t
  let module_ = SemanticTokenTypes.Namespace
  let module_type = SemanticTokenTypes.Interface

  let legend : SemanticTokenTypes.t list =
    [ Namespace
    ; Type
    ; Class
    ; Enum
    ; Interface
    ; Struct
    ; TypeParameter
    ; Parameter
    ; Variable
    ; Property
    ; EnumMember
    ; Event
    ; Function
    ; Method
    ; Macro
    ; Keyword
    ; Modifier
    ; Comment
    ; String
    ; Number
    ; Regexp
    ; Operator
    ; Decorator
    ]
  ;;

  let tokenTypes : string list =
    List.map legend ~f:(fun s ->
      match SemanticTokenTypes.yojson_of_t s with
      | `String s -> s
      | _ -> assert false)
  ;;

  let to_int =
    let module Table = MoreLabels.Hashtbl in
    let table =
      lazy
        (let t = Table.create (List.length legend) in
         List.iteri legend ~f:(fun data key -> Table.add t ~key ~data);
         t)
    in
    fun t -> Table.find (Lazy.force table) t
  ;;

  let to_legend t =
    match SemanticTokenTypes.yojson_of_t t with
    | `String s -> s
    | _ -> assert false
  ;;
end

module Token_modifiers_set : sig
  type t

  val to_int : t -> int
  val singleton : SemanticTokenModifiers.t -> t
  val empty : t
  val list : string list
  val to_legend : t -> string list
end = struct
  type t = int

  let to_int x = x
  let empty = 0

  let singleton : SemanticTokenModifiers.t -> t = function
    | Declaration -> 1 lsl 0
    | Definition -> 1 lsl 1
    | Readonly -> 1 lsl 2
    | Static -> 1 lsl 3
    | Deprecated -> 1 lsl 4
    | Abstract -> 1 lsl 5
    | Async -> 1 lsl 6
    | Modification -> 1 lsl 7
    | Documentation -> 1 lsl 8
    | DefaultLibrary -> 1 lsl 9
  ;;

  let list =
    [ "declaration"
    ; "definition"
    ; "readonly"
    ; "static"
    ; "deprecated"
    ; "abstract"
    ; "async"
    ; "modification"
    ; "documentation"
    ; "defaultLibrary"
    ]
  ;;

  let array = lazy (Array.of_list list)

  let to_legend =
    let cache = lazy (Hashtbl.create 3) in
    fun t ->
      let cache = Lazy.force cache in
      match Hashtbl.find_opt cache t with
      | Some x -> x
      | None ->
        let rec translate t i acc : string list =
          let is_set = Int.equal (t land 1) 1 in
          let t' = t lsr 1 in
          let acc' = if is_set then (Lazy.force array).(i) :: acc else acc in
          if Int.equal t' 0 then List.rev acc' else translate (i + 1) t' acc'
        in
        let res = translate t 0 [] in
        Hashtbl.add cache t res;
        res
  ;;
end

let legend =
  SemanticTokensLegend.create
    ~tokenTypes:Token_type.tokenTypes
    ~tokenModifiers:Token_modifiers_set.list
;;

(** Represents a collection of semantic tokens. *)
module Tokens : sig
  type t

  val create : unit -> t
  val append_token : t -> Loc.t -> Token_type.t -> Token_modifiers_set.t -> unit

  val append_token'
    :  t
    -> Position.t
    -> length:int
    -> Token_type.t
    -> Token_modifiers_set.t
    -> unit

  val yojson_of_t : t -> Yojson.Safe.t
  val encode : t -> int array
end = struct
  type token =
    { start : Position.t
    ; length : int
    ; token_type : Token_type.t
    ; token_modifiers : Token_modifiers_set.t
    }

  type t =
    { mutable tokens : token list (* the last appended token is the head of this list *)
    ; mutable count : int
    }

  let create () : t = { tokens = []; count = 0 }

  let append_token : t -> Loc.t -> Token_type.t -> Token_modifiers_set.t -> unit =
    fun t loc token_type token_modifiers ->
    if loc.loc_ghost
    then ()
    else (
      let range = Range.of_loc_opt loc in
      Option.iter range ~f:(fun ({ start; end_ } : Range.t) ->
        (* TODO: we currently don't handle multi-line range; could handle if
           client supports it - see client's capabilities on initialization *)
        if Int.equal start.line end_.line
        then (
          let new_token : token =
            let length = end_.character - start.character in
            { start; length; token_type; token_modifiers }
          in
          t.tokens <- new_token :: t.tokens;
          t.count <- t.count + 1)))
  ;;

  let append_token'
    : t -> Position.t -> length:int -> Token_type.t -> Token_modifiers_set.t -> unit
    =
    fun t start ~length token_type token_modifiers ->
    let new_token : token = { start; length; token_type; token_modifiers } in
    t.tokens <- new_token :: t.tokens;
    t.count <- t.count + 1
  ;;

  let set_token
    arr
    ~delta_line_index
    ~delta_line
    ~delta_start
    ~length
    ~token_type
    ~token_modifiers
    =
    arr.(delta_line_index) <- delta_line;
    arr.(delta_line_index + 1) <- delta_start;
    arr.(delta_line_index + 2) <- length;
    arr.(delta_line_index + 3) <- token_type;
    arr.(delta_line_index + 4) <- token_modifiers
  ;;

  let yojson_of_token { start; length; token_type; token_modifiers } =
    `Assoc
      [ "start_pos", Position.yojson_of_t start
      ; "length", `Int length
      ; "type", `String (Token_type.to_legend token_type)
      ; ( "modifiers"
        , Json.Conv.yojson_of_list
            Json.Conv.yojson_of_string
            (Token_modifiers_set.to_legend token_modifiers) )
      ]
  ;;

  let yojson_of_t t = Json.Conv.yojson_of_list yojson_of_token (List.rev t.tokens)

  let encode (t : t) : int array =
    let data = Array.init (t.count * 5) ~f:(fun (_ : int) -> 0) in
    let rec aux ix = function
      | [] -> ()
      | [ { start; length; token_type; token_modifiers } ] ->
        set_token
          data
          ~delta_line_index:0
          ~delta_line:start.line
          ~delta_start:start.character
          ~length
          ~token_type:(Token_type.to_int token_type)
          ~token_modifiers:(Token_modifiers_set.to_int token_modifiers)
      | current :: previous :: rest ->
        let delta_line = current.start.line - previous.start.line in
        let delta_start =
          if Int.equal delta_line 0
          then current.start.character - previous.start.character
          else current.start.character
        in
        let { length; token_type; token_modifiers; _ } = current in
        let delta_line_index = (ix - 1) * 5 in
        set_token
          data
          ~delta_line_index
          ~delta_line
          ~delta_start
          ~length
          ~token_type:(Token_type.to_int token_type)
          ~token_modifiers:(Token_modifiers_set.to_int token_modifiers);
        aux (ix - 1) (previous :: rest)
    in
    aux t.count t.tokens;
    data
  ;;
end

(** To traverse OCaml parsetree and produce semantic tokens. *)
module Parsetree_fold (M : sig
    val source : string
  end) : sig
  val apply : Mreader.parsetree -> Tokens.t
end = struct
  (* mutable state *)
  let tokens = Tokens.create ()

  let source_excerpt ({ loc_start; loc_end; _ } : Loc.t) =
    let start_offset = loc_start.pos_cnum in
    let end_offset = loc_end.pos_cnum in
    String.sub M.source ~pos:start_offset ~len:(end_offset - start_offset)
  ;;

  let add_token loc token_type token_modifiers =
    Tokens.append_token tokens loc token_type token_modifiers
  ;;

  let add_token' pos ~length token_type token_modifiers =
    Tokens.append_token' tokens pos ~length token_type token_modifiers
  ;;

  (* TODO: make sure we follow specs when parsing -
     https://v2.ocaml.org/manual/names.html#sss:refer-named *)
  let lident
    ({ loc; _ } : Longident.t Loc.loc)
    rightmost_name
    ?(modifiers = Token_modifiers_set.empty)
    ()
    =
    if loc.loc_ghost
    then ()
    else (
      let start = Position.of_lexical_position loc.loc_start in
      match start with
      | None -> ()
      | Some start ->
        let lid = source_excerpt loc in
        let i = ref 0 in
        let line = ref start.line in
        let character = ref start.character in
        let parse_word () : Position.t * [ `Length of int ] =
          let left_pos = { Position.line = !line; character = !character } in
          while
            !i < String.length lid
            &&
            match lid.[!i] with
            | '\n' | ' ' | '.' -> false
            | _ -> true
          do
            incr character;
            incr i
          done;
          left_pos, `Length (!character - left_pos.character)
        in
        while !i < String.length lid do
          match lid.[!i] with
          | '\n' ->
            incr line;
            character := 0;
            incr i
          | ' ' | '.' ->
            incr character;
            incr i
          | _ ->
            let pos, `Length length = parse_word () in
            let token_type, mods =
              if !i = String.length lid
              then rightmost_name, modifiers
              else Token_type.module_, Token_modifiers_set.empty
            in
            add_token' pos ~length token_type mods
        done)
  ;;

  let constructor_arguments
    (self : Ast_iterator.iterator)
    (ca : Parsetree.constructor_arguments)
    =
    match ca with
    | Pcstr_tuple l -> List.iter l ~f:(fun ct -> self.typ self ct)
    | Pcstr_record l -> List.iter l ~f:(fun r -> self.label_declaration self r)
  ;;

  let module_binding
    (self : Ast_iterator.iterator)
    ({ pmb_name; pmb_expr; pmb_attributes; pmb_loc = _ } : Parsetree.module_binding)
    =
    add_token pmb_name.loc Token_type.module_ (Token_modifiers_set.singleton Definition);
    self.module_expr self pmb_expr;
    self.attributes self pmb_attributes
  ;;

  let typ
    (self : Ast_iterator.iterator)
    ({ ptyp_desc; ptyp_attributes; ptyp_loc; ptyp_loc_stack = _ } as ct :
      Parsetree.core_type)
    =
    let iter =
      match ptyp_desc with
      | Ptyp_var _ ->
        add_token ptyp_loc (Token_type.of_builtin TypeParameter) Token_modifiers_set.empty;
        `Custom_iterator
      | Ptyp_constr (name, cts) | Ptyp_class (name, cts) ->
        List.iter cts ~f:(fun ct -> self.typ self ct);
        lident name (Token_type.of_builtin Type) ();
        `Custom_iterator
      | Ptyp_poly (tps, ct) ->
        List.iter tps ~f:(fun (tp : _ Asttypes.loc) ->
          add_token tp.loc (Token_type.of_builtin TypeParameter) Token_modifiers_set.empty);
        self.typ self ct;
        `Custom_iterator
      | Ptyp_any -> `Custom_iterator
      | Ptyp_variant (_, _, _)
      | Ptyp_alias (_, _)
      | Ptyp_arrow _
      | Ptyp_extension _
      | Ptyp_package _
      | Ptyp_object _
      | Ptyp_tuple _
      | Ptyp_open _ -> `Default_iterator
    in
    match iter with
    | `Default_iterator -> Ast_iterator.default_iterator.typ self ct
    | `Custom_iterator -> self.attributes self ptyp_attributes
  ;;

  let constructor_declaration
    (self : Ast_iterator.iterator)
    ({ pcd_name; pcd_vars; pcd_args; pcd_res; pcd_loc = _; pcd_attributes } :
      Parsetree.constructor_declaration)
    =
    add_token
      pcd_name.loc
      (Token_type.of_builtin EnumMember)
      (Token_modifiers_set.singleton Declaration);
    List.iter pcd_vars ~f:(fun (var : _ Asttypes.loc) ->
      add_token var.loc (Token_type.of_builtin TypeParameter) Token_modifiers_set.empty);
    constructor_arguments self pcd_args;
    Option.iter pcd_res ~f:(fun ct -> self.typ self ct);
    self.attributes self pcd_attributes
  ;;

  let label_declaration
    (self : Ast_iterator.iterator)
    ({ pld_name; pld_mutable = _; pld_type; pld_loc = _; pld_attributes } :
      Parsetree.label_declaration)
    =
    add_token pld_name.loc (Token_type.of_builtin Property) Token_modifiers_set.empty;
    self.typ self pld_type;
    self.attributes self pld_attributes
  ;;

  let value_binding
    (self : Ast_iterator.iterator)
    ({ pvb_pat; pvb_expr; pvb_attributes; _ } as vb : Parsetree.value_binding)
    =
    match
      match pvb_pat.ppat_desc, pvb_expr.pexp_desc with
      | Parsetree.Ppat_var fn_name, _ ->
        (match pvb_expr.pexp_desc with
         | Pexp_function _ ->
           add_token
             fn_name.loc
             (Token_type.of_builtin Function)
             (Token_modifiers_set.singleton Definition);
           self.expr self pvb_expr;
           `Custom_iterator
         | _ -> `Default_iterator)
      | ( Ppat_constraint ({ ppat_desc = Ppat_var n; _ }, pat_ct)
        , Pexp_constraint (e, exp_ct) )
        when Loc.compare pat_ct.ptyp_loc exp_ct.ptyp_loc = 0 ->
        (* handles [let f : t -> unit = fun t -> ()] *)
        add_token
          n.loc
          (match pat_ct.ptyp_desc with
           | Ptyp_poly (_, { ptyp_desc = Ptyp_arrow _; _ }) | Ptyp_arrow _ ->
             Token_type.of_builtin Function
           | _ -> Token_type.of_builtin Variable)
          Token_modifiers_set.empty;
        self.typ self pat_ct;
        self.expr self e;
        `Custom_iterator
      | _ -> `Default_iterator
    with
    | `Default_iterator -> Ast_iterator.default_iterator.value_binding self vb
    | `Custom_iterator -> self.attributes self pvb_attributes
  ;;

  let type_declaration
    (self : Ast_iterator.iterator)
    ({ ptype_name
     ; ptype_params
     ; ptype_cstrs
     ; ptype_kind
     ; ptype_private = _
     ; ptype_manifest
     ; ptype_attributes
     ; ptype_loc = _
     } :
      Parsetree.type_declaration)
    =
    List.iter
      ptype_params
      ~f:
        (fun
          ((core_type, _) :
            Parsetree.core_type * (Asttypes.variance * Asttypes.injectivity))
        ->
        add_token
          core_type.ptyp_loc
          (Token_type.of_builtin TypeParameter)
          Token_modifiers_set.empty);
    add_token
      ptype_name.loc
      (match ptype_kind with
       | Parsetree.Ptype_abstract | Ptype_open -> Token_type.of_builtin Type
       | Ptype_variant _ -> Token_type.of_builtin Enum
       | Ptype_record _ -> Token_type.of_builtin Struct)
      (Token_modifiers_set.singleton Declaration);
    List.iter ptype_cstrs ~f:(fun (ct0, ct1, (_ : Loc.t)) ->
      self.typ self ct0;
      self.typ self ct1);
    Option.iter ptype_manifest ~f:(fun ct -> self.typ self ct);
    (match ptype_kind with
     | Parsetree.Ptype_abstract | Parsetree.Ptype_open -> ()
     | Ptype_variant cds ->
       List.iter cds ~f:(fun cd -> self.constructor_declaration self cd)
     | Ptype_record lds -> List.iter lds ~f:(fun ld -> self.label_declaration self ld));
    self.attributes self ptype_attributes
  ;;

  let const loc (constant : Parsetree.constant) =
    let token_type =
      match constant with
      | Parsetree.Pconst_integer _ | Pconst_float _ -> Token_type.of_builtin Number
      | Pconst_char _ | Pconst_string _ -> Token_type.of_builtin String
    in
    add_token loc token_type Token_modifiers_set.empty
  ;;

  let pexp_apply (self : Ast_iterator.iterator) (expr : Parsetree.expression) args =
    match expr.pexp_desc with
    | Pexp_ident { txt = Ldot (Lident "Array", "set"); _ }
    | Pexp_ident { txt = Ldot (Lident "Array", "get"); _ }
    | Pexp_ident { txt = Ldot (Lident "String", "set"); _ }
    | Pexp_ident { txt = Ldot (Lident "String", "get"); _ } ->
      List.iter args ~f:(fun ((_ : Asttypes.arg_label), e) -> self.expr self e);
      `Custom_iterator
    | Pexp_ident lid ->
      (match args with
       | (_, fst_arg) :: rest
         when (* true if applied function is infix, i.e., function name occurs
                 after the first argument *)
              Loc.compare lid.loc fst_arg.pexp_loc > 0 ->
         self.expr self fst_arg;
         (* [lident] parses the identifier to find module names, which we don't
            need to do for infix operators. *)
         add_token lid.loc (Token_type.of_builtin Function) Token_modifiers_set.empty;
         List.iter rest ~f:(fun (_, e) -> self.expr self e)
       | _ ->
         lident lid (Token_type.of_builtin Function) ();
         List.iter args ~f:(fun (_, e) -> self.expr self e));
      `Custom_iterator
    | Pexp_field (e, l) ->
      self.expr self e;
      lident l (Token_type.of_builtin Function) ();
      `Custom_iterator
    | _ -> `Default_iterator
  ;;

  let expr
    (self : Ast_iterator.iterator)
    ({ pexp_desc; pexp_loc; pexp_loc_stack = _; pexp_attributes } as exp :
      Parsetree.expression)
    =
    match
      match pexp_desc with
      | Parsetree.Pexp_ident l ->
        lident l (Token_type.of_builtin Variable) ();
        `Custom_iterator
      | Pexp_construct (c, vo) ->
        (match c.txt with
         | Lident "::" ->
           (* because [a; b] is desugared to [Pexp_construct (Lident "::",
             Pexp_tuple(...))] *)
           Option.iter vo ~f:(fun v -> self.expr self v)
         | Lident "[]" -> () (* TDOO: is this correct? *)
         | Lident "()" -> ()
         | _ ->
           lident c (Token_type.of_builtin EnumMember) ();
           Option.iter vo ~f:(fun v -> self.expr self v));
        `Custom_iterator
      | Pexp_apply (expr, args) -> pexp_apply self expr args
      | Pexp_function _ | Pexp_let (_, _, _) -> `Default_iterator
      | Pexp_try (_, _)
      | Pexp_tuple _
      | Pexp_variant (_, _)
      (* ^ label for a poly variant is missing location info -- we could have a
         workaround by "parsing" this part of code ourselves*)
      | Pexp_match (_, _) -> `Default_iterator
      | Pexp_record (props, exp) ->
        Option.iter exp ~f:(fun e -> self.expr self e);
        List.iter props ~f:(fun (lid, (exp : Parsetree.expression)) ->
          lident lid (Token_type.of_builtin Property) ();
          if Loc.compare lid.loc exp.pexp_loc <> 0 (* handles field punning *)
          then self.expr self exp);
        `Custom_iterator
      | Pexp_field (e, l) ->
        self.expr self e;
        lident l (Token_type.of_builtin Property) ();
        `Custom_iterator
      | Pexp_send (e, m) ->
        self.expr self e;
        add_token m.loc (Token_type.of_builtin Method) Token_modifiers_set.empty;
        `Custom_iterator
      | Pexp_setfield (e0, l, e1) ->
        self.expr self e0;
        lident l (Token_type.of_builtin Variable) ();
        self.expr self e1;
        `Custom_iterator
      | Pexp_new l ->
        lident l (Token_type.of_builtin Class) ();
        `Custom_iterator
      | Pexp_newtype (t, e) ->
        add_token t.loc (Token_type.of_builtin TypeParameter) Token_modifiers_set.empty;
        self.expr self e;
        `Custom_iterator
      | Pexp_letmodule (name, me, e) ->
        add_token name.loc Token_type.module_ Token_modifiers_set.empty;
        self.module_expr self me;
        (* ^ handle function applications like this *)
        self.expr self e;
        `Custom_iterator
      | Pexp_constant c ->
        const pexp_loc c;
        `Custom_iterator
      | Pexp_sequence (e0, e1) ->
        self.expr self e0;
        self.expr self e1;
        `Custom_iterator
      | Pexp_constraint (e, ct) ->
        (* handles [let f () : int = 1] and [let f () = (1 : int)] *)
        if Loc.compare e.pexp_loc ct.ptyp_loc > 0
        then (
          self.typ self ct;
          self.expr self e)
        else (
          self.expr self e;
          self.typ self ct);
        `Custom_iterator
      | Pexp_letop { let_; ands; body } ->
        List.iter
          (let_ :: ands)
          ~f:(fun { Parsetree.pbop_op = _; pbop_pat; pbop_exp; pbop_loc = _ } ->
            self.pat self pbop_pat;
            if Loc.compare pbop_pat.ppat_loc pbop_exp.pexp_loc
               <> 0 (* handles punning as in e.g. [let* foo in <expr>]*)
            then self.expr self pbop_exp);
        self.expr self body;
        `Custom_iterator
      | Pexp_unreachable -> `Custom_iterator
      | Pexp_array _
      | Pexp_ifthenelse (_, _, _)
      | Pexp_while (_, _)
      | Pexp_for (_, _, _, _, _)
      | Pexp_coerce (_, _, _)
      | Pexp_setinstvar (_, _)
      | Pexp_override _
      | Pexp_letexception (_, _)
      | Pexp_assert _ | Pexp_lazy _
      | Pexp_poly (_, _)
      | Pexp_object _ | Pexp_pack _
      | Pexp_open (_, _)
      | Pexp_extension _ -> `Default_iterator
    with
    | `Default_iterator -> Ast_iterator.default_iterator.expr self exp
    | `Custom_iterator -> self.attributes self pexp_attributes
  ;;

  let pat
    (self : Ast_iterator.iterator)
    ({ ppat_desc; ppat_loc; ppat_loc_stack = _; ppat_attributes } as pat :
      Parsetree.pattern)
    =
    match
      match ppat_desc with
      | Parsetree.Ppat_var v ->
        add_token v.loc (Token_type.of_builtin Variable) Token_modifiers_set.empty;
        `Custom_iterator
      | Ppat_alias (p, a) ->
        self.pat self p;
        add_token a.loc (Token_type.of_builtin Variable) Token_modifiers_set.empty;
        `Custom_iterator
      | Ppat_construct (c, args) ->
        let process_args () =
          Option.iter args ~f:(fun (tvs, pat) ->
            List.iter tvs ~f:(fun (tv : _ Asttypes.loc) ->
              add_token
                tv.loc
                (Token_type.of_builtin TypeParameter)
                Token_modifiers_set.empty);
            self.pat self pat)
        in
        (match c.txt with
         | Lident "::" -> process_args ()
         | Lident "[]" -> ()
         | Lident "()" -> ()
         | _ ->
           lident c (Token_type.of_builtin EnumMember) ();
           process_args ());
        `Custom_iterator
      | Ppat_constant c ->
        const ppat_loc c;
        `Custom_iterator
      | Ppat_open (lid, p) ->
        lident lid Token_type.module_ ();
        self.pat self p;
        `Custom_iterator
      | Ppat_unpack m ->
        Option.iter m.txt ~f:(fun _ ->
          add_token m.loc Token_type.module_ Token_modifiers_set.empty);
        `Custom_iterator
      | Ppat_type t ->
        lident t (Token_type.of_builtin Type) ();
        `Custom_iterator
      | Ppat_record (flds, _) ->
        List.iter flds ~f:(fun (fld, (pat : Parsetree.pattern)) ->
          lident fld (Token_type.of_builtin Property) ();
          if Loc.compare fld.loc pat.ppat_loc <> 0 (* handles field punning *)
          then self.pat self pat);
        `Custom_iterator
      | Ppat_constraint (p, ct) ->
        self.pat self p;
        self.typ self ct;
        `Custom_iterator
      | Ppat_or _
      | Ppat_exception _
      | Ppat_variant _
      | Ppat_array _
      | Ppat_extension _
      | Ppat_tuple _
      | Ppat_lazy _
      | Ppat_any
      | Ppat_interval _ -> `Default_iterator
    with
    | `Default_iterator -> Ast_iterator.default_iterator.pat self pat
    | `Custom_iterator -> self.attributes self ppat_attributes
  ;;

  let module_expr
    (self : Ast_iterator.iterator)
    ({ pmod_desc; pmod_loc = _; pmod_attributes } as me : Parsetree.module_expr)
    =
    match
      match pmod_desc with
      | Pmod_ident s ->
        lident s Token_type.module_ ();
        `Custom_iterator
      | Pmod_functor (fp, me) ->
        (match fp with
         | Unit -> ()
         | Named (n, mt) ->
           add_token n.loc Token_type.module_ Token_modifiers_set.empty;
           self.module_type self mt);
        self.module_expr self me;
        `Custom_iterator
      | Pmod_constraint (me, mt) ->
        if Loc.compare me.pmod_loc mt.pmty_loc > 0
        then (
          self.module_type self mt;
          self.module_expr self me)
        else (
          self.module_expr self me;
          self.module_type self mt);
        `Custom_iterator
      | Pmod_extension _ -> `Custom_iterator
      | _ ->
        (* We rely on the wildcard pattern to improve compatibility with
           multiple OCaml's parsetree versions *)
        `Default_iterator
    with
    | `Custom_iterator -> self.attributes self pmod_attributes
    | `Default_iterator -> Ast_iterator.default_iterator.module_expr self me
  ;;

  let module_type_declaration
    (self : Ast_iterator.iterator)
    ({ pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc = _ } :
      Parsetree.module_type_declaration)
    =
    add_token pmtd_name.loc Token_type.module_type Token_modifiers_set.empty;
    Option.iter pmtd_type ~f:(fun mdtt -> self.module_type self mdtt);
    self.attributes self pmtd_attributes
  ;;

  let value_description
    (self : Ast_iterator.iterator)
    ({ pval_name; pval_type; pval_prim = _; pval_attributes; pval_loc = _ } :
      Parsetree.value_description)
    =
    add_token
      pval_name.loc
      (match pval_type.ptyp_desc with
       | Ptyp_arrow (_, _, _) -> Token_type.of_builtin Function
       | Ptyp_class (_, _) -> Token_type.of_builtin Class
       | Ptyp_package _ -> Token_type.module_
       | Ptyp_extension _
       | Ptyp_constr (_, _)
       | Ptyp_object (_, _)
       | Ptyp_alias (_, _)
       | Ptyp_variant (_, _, _)
       | Ptyp_poly (_, _)
       | Ptyp_tuple _ | Ptyp_any | Ptyp_var _ | Ptyp_open _ ->
         Token_type.of_builtin Variable)
      (Token_modifiers_set.singleton Declaration);
    self.typ self pval_type;
    (* TODO: handle pval_prim ? *)
    self.attributes self pval_attributes
  ;;

  let module_declaration
    (self : Ast_iterator.iterator)
    ({ pmd_name; pmd_type; pmd_attributes; pmd_loc = _ } : Parsetree.module_declaration)
    =
    add_token pmd_name.loc Token_type.module_ (Token_modifiers_set.singleton Declaration);
    self.module_type self pmd_type;
    self.attributes self pmd_attributes
  ;;

  let module_type (self : Ast_iterator.iterator) (mt : Parsetree.module_type) =
    match
      match mt.pmty_desc with
      | Pmty_ident l ->
        lident l Token_type.module_type ();
        `Custom_iterator
      | Pmty_functor (fp, mt) ->
        (match fp with
         | Unit -> ()
         | Named (n, mt) ->
           add_token n.loc Token_type.module_ Token_modifiers_set.empty;
           self.module_type self mt);
        self.module_type self mt;
        `Custom_iterator
      | Pmty_alias m ->
        lident m Token_type.module_ ();
        `Custom_iterator
      | Pmty_signature sis ->
        List.iter sis ~f:(fun si -> self.signature_item self si);
        `Custom_iterator
      | Pmty_with (_, _) | Pmty_typeof _ | Pmty_extension _ -> `Default_iterator
    with
    | `Custom_iterator -> ()
    | `Default_iterator -> Ast_iterator.default_iterator.module_type self mt
  ;;

  (* TODO: *)
  let attribute _self _attr = ()

  (* TODO: *)
  let attributes _self _attrs = ()

  let iterator =
    { Ast_iterator.default_iterator with
      module_binding
    ; type_declaration
    ; expr
    ; pat
    ; constructor_declaration
    ; label_declaration
    ; typ
    ; value_binding
    ; module_type_declaration
    ; attribute
    ; attributes
    ; module_expr
    ; value_description
    ; module_type
    ; module_declaration
    }
  ;;

  let apply parsetree =
    (match parsetree with
     | `Interface signature -> iterator.signature iterator signature
     | `Implementation structure -> iterator.structure iterator structure);
    tokens
  ;;
end

(** File-wide mutable state that allows to generate unique IDs for semantic
    tokens requests (both [full] and [full/delta]) *)
let gen_new_id =
  let i = ref 0 in
  fun () ->
    let x = !i in
    incr i;
    string_of_int x
;;

let compute_tokens doc =
  let+ parsetree, source =
    Document.Merlin.with_pipeline_exn ~name:"semantic highlighting" doc (fun p ->
      Mpipeline.reader_parsetree p, Mpipeline.input_source p)
  in
  let module Fold =
    Parsetree_fold (struct
      let source = Msource.text source
    end)
  in
  Fold.apply parsetree
;;

let compute_encoded_tokens doc =
  let+ tokens = compute_tokens doc in
  Tokens.encode tokens
;;

(** Contains implementation of a custom request that provides human-readable
    tokens representation *)
module Debug = struct
  let meth_request_full = "ocamllsp/textDocument/semanticTokens/full"

  let on_request_full : params:Jsonrpc.Structured.t option -> State.t -> Json.t Fiber.t =
    fun ~params state ->
    Fiber.of_thunk (fun () ->
      match params with
      | None ->
        Jsonrpc.Response.Error.raise
        @@ Jsonrpc.Response.Error.make
             ~code:Jsonrpc.Response.Error.Code.InvalidParams
             ~message:(meth_request_full ^ " expects an argument but didn't receive any")
             ()
      | Some (`Assoc _ as json) | Some (`List _ as json) ->
        let params = SemanticTokensParams.t_of_yojson json in
        let store = state.store in
        let uri = params.textDocument.uri in
        let doc = Document_store.get store uri in
        (match Document.kind doc with
         | `Other ->
           Jsonrpc.Response.Error.raise
           @@ Jsonrpc.Response.Error.make
                ~code:Jsonrpc.Response.Error.Code.InvalidParams
                ~message:"expected a merlin document"
                ()
         | `Merlin merlin ->
           let+ tokens = compute_tokens merlin in
           Tokens.yojson_of_t tokens))
  ;;
end

let on_request_full : State.t -> SemanticTokensParams.t -> SemanticTokens.t option Fiber.t
  =
  fun state params ->
  Fiber.of_thunk (fun () ->
    let store = state.store in
    let uri = params.textDocument.uri in
    let doc = Document_store.get store uri in
    match Document.kind doc with
    | `Other -> Fiber.return None
    | `Merlin doc ->
      let+ tokens = compute_encoded_tokens doc in
      let resultId = gen_new_id () in
      Document_store.update_semantic_tokens_cache store uri ~resultId ~tokens;
      Some { SemanticTokens.resultId = Some resultId; data = tokens })
;;

(* TODO: refactor [find_diff] and write (inline?) tests *)

(* [find_diff] finds common prefix and common suffix and reports the rest as
   array difference. This is not ideal but good enough. The idea comes from the
   Rust Analyzer implementation of this function. *)
let find_diff ~(old : int array) ~(new_ : int array) : SemanticTokensEdit.t list =
  let old_len = Array.length old in
  let new_len = Array.length new_ in
  let left_offset = Array.common_prefix_len ~equal:Int.equal old new_ in
  if left_offset = old_len
  then
    if left_offset = new_len
    then (* [old] and [new_] are simply equal *) []
    else
      (* [old] is prefix of [new_] *)
      [ SemanticTokensEdit.create
          ~start:left_offset
          ~deleteCount:0
          ~data:(Array.sub new_ ~pos:left_offset ~len:(new_len - left_offset))
          ()
      ]
  else if left_offset = new_len
  then
    (* [new_] is prefix of [old] *)
    [ SemanticTokensEdit.create ~start:left_offset ~deleteCount:(old_len - left_offset) ()
    ]
  else (
    let common_suffix_len =
      let old_noncommon = Array_view.make old ~pos:left_offset in
      let new_noncommon = Array_view.make new_ ~pos:left_offset in
      Array_view.common_suffix_len old_noncommon new_noncommon
    in
    let deleteCount =
      let right_offset_old = old_len - common_suffix_len in
      right_offset_old - left_offset
    in
    let data =
      let right_offset_new = new_len - common_suffix_len in
      Array.sub new_ ~pos:left_offset ~len:(right_offset_new - left_offset)
    in
    [ SemanticTokensEdit.create ~start:left_offset ~deleteCount ~data () ])
;;

let on_request_full_delta
  :  State.t -> SemanticTokensDeltaParams.t
  -> [ `SemanticTokens of SemanticTokens.t
     | `SemanticTokensDelta of SemanticTokensDelta.t
     ]
       option
       Fiber.t
  =
  fun state params ->
  Fiber.of_thunk (fun () ->
    let store = state.store in
    let uri = params.textDocument.uri in
    let doc = Document_store.get store uri in
    match Document.kind doc with
    | `Other -> Fiber.return None
    | `Merlin doc ->
      let+ tokens = compute_encoded_tokens doc in
      let resultId = gen_new_id () in
      let cached_token_info =
        Document_store.get_semantic_tokens_cache state.store params.textDocument.uri
      in
      Document_store.update_semantic_tokens_cache store uri ~resultId ~tokens;
      (match cached_token_info with
       | Some cached_v when String.equal cached_v.resultId params.previousResultId ->
         let edits = find_diff ~old:cached_v.tokens ~new_:tokens in
         Some
           (`SemanticTokensDelta { SemanticTokensDelta.resultId = Some resultId; edits })
       | Some _ | None ->
         Some (`SemanticTokens { SemanticTokens.resultId = Some resultId; data = tokens })))
;;
