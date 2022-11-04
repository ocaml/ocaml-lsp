open Import
open Fiber.O

(* TODO:

   - [x] support textDocument/semanticTokens/full

   - [x] support textDocument/semanticTokens/full/delta

   - [ ] support textDocument/semanticTokens/range *)

module Token_type : sig
  type t = private int

  val module_ : t

  val type_ : t

  val class_ : t

  val enum : t

  val module_type : t

  val record : t

  val type_parameter : t

  val parameter : t

  val variable : t

  val property : t

  val enum_member : t

  val event : t

  val function_ : t

  val method_ : t

  val macro : t

  val keyword : t

  val modifier : t

  val comment : t

  val string : t

  val number : t

  val regexp : t

  val operator : t

  val decorator : t

  val list : string list

  val to_legend : t -> string
end [@warning "-32"] = struct
  type t = int

  let module_ = 0
  (* the number is determined by the index in [list] defined below; e.g., we use
     [0] here because we'd like to use "namespace" token type for module
     symbols *)

  let type_ = 1

  let class_ = 2

  let enum = 3

  let module_type = 4

  let record = 5

  let type_parameter = 6

  let parameter = 7

  let variable = 8

  let property = 9

  let enum_member = 10

  let event = 11

  let function_ = 12

  let method_ = 13

  let macro = 14

  let keyword = 15

  let modifier = 16

  let comment = 17

  let string = 18

  let number = 19

  let regexp = 20

  let operator = 21

  let decorator = 22

  let list =
    [ "namespace"
    ; "type"
    ; "class"
    ; "enum"
    ; "interface"
    ; "struct"
    ; "typeParameter"
    ; "parameter"
    ; "variable"
    ; "property"
    ; "enumMember"
    ; "event"
    ; "function"
    ; "method"
    ; "macro"
    ; "keyword"
    ; "modifier"
    ; "comment"
    ; "string"
    ; "number"
    ; "regexp"
    ; "operator"
    ; "decorator"
    ]

  let array = lazy (Array.of_list list)

  let to_legend t = (Lazy.force array).(t)
end

module Token_modifiers_set : sig
  type t = private int

  val singleton : SemanticTokenModifiers.t -> t

  val empty : t

  val list : string list

  val to_legend : t -> string list
end = struct
  type t = int

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
end

let legend =
  SemanticTokensLegend.create
    ~tokenTypes:Token_type.list
    ~tokenModifiers:Token_modifiers_set.list

module Tokens : sig
  type t

  val create : unit -> t

  val append_token : t -> Loc.t -> Token_type.t -> Token_modifiers_set.t -> unit

  val append_token' :
       t
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
    { mutable tokens :
        token list (* the last appended token is the head of this list *)
    ; mutable count : int
    }

  let create () : t = { tokens = []; count = 0 }

  let append_token : t -> Loc.t -> Token_type.t -> Token_modifiers_set.t -> unit
      =
   fun t loc token_type token_modifiers ->
    let range = Range.of_loc_opt loc in
    Option.iter range ~f:(fun ({ start; end_ } : Range.t) ->
        (* TODO: we currently don't handle multi-line range; could handle if
           client supports it - see client's capabilities on initialization *)
        if Int.equal start.line end_.line then (
          let new_token : token =
            let length = end_.character - start.character in
            { start; length; token_type; token_modifiers }
          in
          t.tokens <- new_token :: t.tokens;
          t.count <- t.count + 1))

  let append_token' :
         t
      -> Position.t
      -> length:int
      -> Token_type.t
      -> Token_modifiers_set.t
      -> unit =
   fun t start ~length token_type token_modifiers ->
    let new_token : token = { start; length; token_type; token_modifiers } in
    t.tokens <- new_token :: t.tokens;
    t.count <- t.count + 1

  let set_token arr ~delta_line_index ~delta_line ~delta_start ~length
      ~token_type ~token_modifiers =
    arr.(delta_line_index) <- delta_line;
    arr.(delta_line_index + 1) <- delta_start;
    arr.(delta_line_index + 2) <- length;
    arr.(delta_line_index + 3) <- token_type;
    arr.(delta_line_index + 4) <- token_modifiers

  let yojson_of_token { start; length; token_type; token_modifiers } =
    `Assoc
      [ ("start_pos", Position.yojson_of_t start)
      ; ("length", `Int length)
      ; ("type", `String (Token_type.to_legend token_type))
      ; ( "modifiers"
        , Json.Conv.yojson_of_list
            Json.Conv.yojson_of_string
            (Token_modifiers_set.to_legend token_modifiers) )
      ]

  let yojson_of_t t =
    Json.Conv.yojson_of_list yojson_of_token (List.rev t.tokens)

  let encode (t : t) : int array =
    let data = Array.init (t.count * 5) ~f:(fun (_ : int) -> 0) in
    let rec aux ix = function
      | [] -> ()
      | [ { start; length; token_type; token_modifiers } ] ->
        set_token
          data
          ~delta_line_index:0
          ~delta_line:start.Position.line
          ~delta_start:start.character
          ~length
          ~token_type:(token_type :> int)
          ~token_modifiers:(token_modifiers :> int)
      | current :: previous :: rest ->
        let delta_line = current.start.line - previous.start.line in
        let delta_start =
          if Int.equal delta_line 0 then
            current.start.character - previous.start.character
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
          ~token_type:(token_type :> int)
          ~token_modifiers:(token_modifiers :> int);
        aux (ix - 1) (previous :: rest)
    in
    aux t.count t.tokens;
    data
end

module Parsetree_fold () = struct
  (* mutable state *)
  let tokens = Tokens.create ()

  let add_token loc token_type token_modifiers =
    Tokens.append_token tokens loc token_type token_modifiers

  let add_token' pos ~length token_type token_modifiers =
    Tokens.append_token' tokens pos ~length token_type token_modifiers

  let lident ({ txt = lid; loc } : Longident.t Loc.loc) rightmost_name
      ?(modifiers = Token_modifiers_set.empty) () =
    let start : Position.t option =
      Position.of_lexical_position loc.loc_start
    in
    Option.iter start ~f:(fun start ->
        let lst = Longident.flatten lid in
        let rec aux offset = function
          | [] -> ()
          | [ constr_name ] ->
            add_token'
              { start with character = start.character + offset }
              ~length:(String.length constr_name)
              rightmost_name
              modifiers
          | mod_name :: rest ->
            add_token'
              { start with character = start.character + offset }
              ~length:(String.length mod_name)
              Token_type.module_
              Token_modifiers_set.empty;
            aux (offset + String.length mod_name + 1) rest
        in
        aux 0 lst)

  let constructor_arguments (self : Ast_iterator.iterator)
      (ca : Parsetree.constructor_arguments) =
    match ca with
    | Pcstr_tuple l -> List.iter l ~f:(fun ct -> self.typ self ct)
    | Pcstr_record l -> List.iter l ~f:(fun r -> self.label_declaration self r)

  let module_binding (self : Ast_iterator.iterator)
      ({ pmb_name; pmb_expr; pmb_attributes; pmb_loc = _ } :
        Parsetree.module_binding) =
    add_token
      pmb_name.loc
      Token_type.module_
      (Token_modifiers_set.singleton Definition);
    self.module_expr self pmb_expr;
    self.attributes self pmb_attributes

  let typ (self : Ast_iterator.iterator)
      ({ ptyp_desc; ptyp_attributes; ptyp_loc; ptyp_loc_stack = _ } as ct :
        Parsetree.core_type) =
    let iter =
      match ptyp_desc with
      | Ptyp_var _ ->
        add_token ptyp_loc Token_type.type_parameter Token_modifiers_set.empty;
        `Custom_iterator
      | Ptyp_constr (name, cts) | Ptyp_class (name, cts) ->
        List.iter cts ~f:(fun ct -> self.typ self ct);
        lident name Token_type.type_ ();
        `Custom_iterator
      | Ptyp_poly (tps, ct) ->
        List.iter tps ~f:(fun tp ->
            add_token
              tp.Loc.loc
              Token_type.type_parameter
              Token_modifiers_set.empty);
        self.typ self ct;
        `Custom_iterator
      | Ptyp_variant (_, _, _)
      | Ptyp_alias (_, _)
      | Ptyp_arrow _
      | Ptyp_extension _
      | Ptyp_package _
      | Ptyp_object _
      | Ptyp_tuple _ -> `Default_iterator
      | Ptyp_any ->
        ();
        `Custom_iterator
    in
    match iter with
    | `Default_iterator -> Ast_iterator.default_iterator.typ self ct
    | `Custom_iterator -> self.attributes self ptyp_attributes

  (* COMPLETE *)
  let constructor_declaration (self : Ast_iterator.iterator)
      ({ pcd_name; pcd_vars; pcd_args; pcd_res; pcd_loc = _; pcd_attributes } :
        Parsetree.constructor_declaration) =
    add_token
      pcd_name.loc
      Token_type.enum_member
      (Token_modifiers_set.singleton Declaration);
    List.iter pcd_vars ~f:(fun var ->
        add_token
          var.Loc.loc
          Token_type.type_parameter
          Token_modifiers_set.empty);
    constructor_arguments self pcd_args;
    Option.iter pcd_res ~f:(fun ct -> self.typ self ct);
    self.attributes self pcd_attributes

  (* COMPLETE *)
  let label_declaration (self : Ast_iterator.iterator)
      ({ pld_name; pld_mutable = _; pld_type; pld_loc = _; pld_attributes } :
        Parsetree.label_declaration) =
    add_token pld_name.loc Token_type.property Token_modifiers_set.empty;
    self.typ self pld_type;
    self.attributes self pld_attributes

  let value_binding (self : Ast_iterator.iterator)
      ({ pvb_pat; pvb_expr; pvb_attributes; pvb_loc = _ } as vb :
        Parsetree.value_binding) =
    match
      match (pvb_pat.ppat_desc, pvb_expr.pexp_desc) with
      | Parsetree.Ppat_var fn_name, _ -> (
        match pvb_expr.pexp_desc with
        | Pexp_fun _ | Pexp_function _ ->
          add_token
            fn_name.loc
            Token_type.function_
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
            Token_type.function_
          | _ -> Token_type.variable)
          Token_modifiers_set.empty;
        self.typ self pat_ct;
        self.expr self e;
        `Custom_iterator
      | _ -> `Default_iterator
    with
    | `Default_iterator -> Ast_iterator.default_iterator.value_binding self vb
    | `Custom_iterator -> self.attributes self pvb_attributes

  let type_declaration (self : Ast_iterator.iterator)
      ({ ptype_name
       ; ptype_params
       ; ptype_cstrs
       ; ptype_kind
       ; ptype_private = _
       ; ptype_manifest
       ; ptype_attributes
       ; ptype_loc = _
       } :
        Parsetree.type_declaration) =
    List.iter
      ptype_params
      ~f:(fun
           ((core_type, _) :
             Parsetree.core_type * (Asttypes.variance * Asttypes.injectivity))
         ->
        add_token
          core_type.ptyp_loc
          Token_type.type_parameter
          Token_modifiers_set.empty);
    add_token
      ptype_name.loc
      (match ptype_kind with
      | Parsetree.Ptype_abstract | Ptype_open -> Token_type.type_
      | Ptype_variant _ -> Token_type.enum
      | Ptype_record _ -> Token_type.record)
      (Token_modifiers_set.singleton Declaration);
    List.iter ptype_cstrs ~f:(fun (ct0, ct1, (_ : Loc.t)) ->
        self.typ self ct0;
        self.typ self ct1);
    Option.iter ptype_manifest ~f:(fun ct -> self.typ self ct);
    (match ptype_kind with
    | Parsetree.Ptype_abstract | Parsetree.Ptype_open -> ()
    | Ptype_variant cds ->
      List.iter cds ~f:(fun cd -> self.constructor_declaration self cd)
    | Ptype_record lds ->
      List.iter lds ~f:(fun ld -> self.label_declaration self ld));
    self.attributes self ptype_attributes

  let const loc = function
    | Parsetree.Pconst_integer _ | Pconst_float _ ->
      add_token loc Token_type.number Token_modifiers_set.empty
    | Pconst_char _ | Pconst_string _ ->
      add_token loc Token_type.string Token_modifiers_set.empty

  let pexp_apply (self : Ast_iterator.iterator) (expr : Parsetree.expression)
      args =
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
        lident lid Token_type.function_ ();
        List.iter rest ~f:(fun (_, e) -> self.expr self e)
      | _ ->
        lident lid Token_type.function_ ();
        List.iter args ~f:(fun (_, e) -> self.expr self e));
      `Custom_iterator
    | Pexp_field (e, l) ->
      self.expr self e;
      lident l Token_type.function_ ();
      `Custom_iterator
    | _ -> `Default_iterator

  let expr (self : Ast_iterator.iterator)
      ({ pexp_desc; pexp_loc; pexp_loc_stack = _; pexp_attributes } as exp :
        Parsetree.expression) =
    match
      match pexp_desc with
      | Parsetree.Pexp_ident l ->
        lident l Token_type.variable ();
        `Custom_iterator
      | Pexp_construct (c, vo) ->
        (match c.txt with
        | Lident "::" ->
          (* because [a; b] is desugared to [Pexp_construct (Lident "::",
             Pexp_tuple(...))] *)
          Option.iter vo ~f:(fun v -> self.expr self v)
        | Lident "[]" -> () (* TDOO: is this correct? *)
        | _ ->
          lident c Token_type.enum_member ();
          Option.iter vo ~f:(fun v -> self.expr self v));
        `Custom_iterator
      | Pexp_apply (expr, args) -> pexp_apply self expr args
      | Pexp_function _ | Pexp_let (_, _, _) -> `Default_iterator
      | Pexp_fun (_, expr_opt, pat, expr) ->
        (match expr_opt with
        | None -> self.pat self pat
        | Some e ->
          if Loc.compare e.pexp_loc pat.ppat_loc < 0 then (
            self.expr self e;
            self.pat self pat)
          else (
            self.pat self pat;
            self.expr self e));
        self.expr self expr;
        `Custom_iterator
      | Pexp_try (_, _)
      | Pexp_tuple _
      | Pexp_variant (_, _)
      (* ^ label is missing location info -- we could have a workaround by
         "parsing" this part of code ourselves*)
      | Pexp_match (_, _) -> `Default_iterator
      | Pexp_record (props, exp) ->
        Option.iter exp ~f:(fun e -> self.expr self e);
        List.iter props ~f:(fun (lid, exp) ->
            lident lid Token_type.property ();
            if
              Loc.compare lid.Loc.loc exp.Parsetree.pexp_loc
              <> 0 (* handles field punning *)
            then self.expr self exp);
        `Custom_iterator
      | Pexp_field (e, l) ->
        self.expr self e;
        lident l Token_type.property ();
        `Custom_iterator
      | Pexp_send (e, m) ->
        self.expr self e;
        add_token m.loc Token_type.method_ Token_modifiers_set.empty;
        `Custom_iterator
      | Pexp_setfield (e0, l, e1) ->
        self.expr self e0;
        lident l Token_type.variable ();
        self.expr self e1;
        `Custom_iterator
      | Pexp_new l ->
        lident l Token_type.class_ ();
        `Custom_iterator
      | Pexp_newtype (t, e) ->
        add_token t.loc Token_type.type_parameter Token_modifiers_set.empty;
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
        if Loc.compare e.pexp_loc ct.ptyp_loc > 0 then (
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
            if
              Loc.compare pbop_pat.ppat_loc pbop_exp.pexp_loc
              <> 0 (* handles punning as in e.g. [let* foo in <expr>]*)
            then self.expr self pbop_exp);
        self.expr self body;
        `Custom_iterator
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
      | Pexp_unreachable | Pexp_hole -> `Custom_iterator
    with
    | `Default_iterator -> Ast_iterator.default_iterator.expr self exp
    | `Custom_iterator -> self.attributes self pexp_attributes

  (* TODO: handle poly variants *)

  let pat (self : Ast_iterator.iterator)
      ({ ppat_desc; ppat_loc; ppat_loc_stack = _; ppat_attributes } as pat :
        Parsetree.pattern) =
    match
      match ppat_desc with
      | Parsetree.Ppat_var v ->
        add_token v.loc Token_type.variable Token_modifiers_set.empty;
        `Custom_iterator
      | Ppat_alias (p, a) ->
        self.pat self p;
        add_token a.loc Token_type.variable Token_modifiers_set.empty;
        `Custom_iterator
      | Ppat_construct (c, args) ->
        let process_args () =
          Option.iter args ~f:(fun (tvs, pat) ->
              List.iter tvs ~f:(fun tv ->
                  add_token
                    tv.Loc.loc
                    Token_type.type_parameter
                    Token_modifiers_set.empty);
              self.pat self pat)
        in
        (match c.txt with
        | Lident "::" -> process_args ()
        | Lident "[]" -> ()
        | _ ->
          lident c Token_type.enum_member ();
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
        lident t Token_type.type_ ();
        `Custom_iterator
      | Ppat_record (flds, _) ->
        List.iter flds ~f:(fun (fld, (pat : Parsetree.pattern)) ->
            lident fld Token_type.property ();
            if
              Loc.compare fld.Loc.loc pat.ppat_loc
              <> 0 (* handles field punning *)
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

  let module_expr (self : Ast_iterator.iterator)
      ({ pmod_desc; pmod_loc = _; pmod_attributes } as me :
        Parsetree.module_expr) =
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
        if Loc.compare me.pmod_loc mt.pmty_loc > 0 then (
          self.module_type self mt;
          self.module_expr self me)
        else (
          self.module_expr self me;
          self.module_type self mt);
        `Custom_iterator
      | Pmod_extension _ | Pmod_hole -> `Custom_iterator
      | Pmod_unpack _ | Pmod_apply (_, _) | Pmod_structure _ ->
        `Default_iterator
    with
    | `Custom_iterator -> self.attributes self pmod_attributes
    | `Default_iterator -> Ast_iterator.default_iterator.module_expr self me

  let module_type_declaration (self : Ast_iterator.iterator)
      ({ pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc = _ } :
        Parsetree.module_type_declaration) =
    add_token pmtd_name.loc Token_type.module_type Token_modifiers_set.empty;
    Option.iter pmtd_type ~f:(fun mdtt -> self.module_type self mdtt);
    self.attributes self pmtd_attributes

  let value_description (self : Ast_iterator.iterator)
      ({ pval_name; pval_type; pval_prim = _; pval_attributes; pval_loc = _ } :
        Parsetree.value_description) =
    add_token
      pval_name.Loc.loc
      (match pval_type.ptyp_desc with
      | Ptyp_arrow (_, _, _) -> Token_type.function_
      | Ptyp_class (_, _) -> Token_type.class_
      | Ptyp_package _ -> Token_type.module_
      | Ptyp_extension _
      | Ptyp_constr (_, _)
      | Ptyp_object (_, _)
      | Ptyp_alias (_, _)
      | Ptyp_variant (_, _, _)
      | Ptyp_poly (_, _)
      | Ptyp_tuple _ | Ptyp_any | Ptyp_var _ -> Token_type.variable)
      (Token_modifiers_set.singleton Declaration);
    self.typ self pval_type;
    (* TODO: handle pval_prim ? *)
    self.attributes self pval_attributes

  let module_declaration (self : Ast_iterator.iterator)
      ({ pmd_name; pmd_type; pmd_attributes; pmd_loc = _ } :
        Parsetree.module_declaration) =
    add_token
      pmd_name.loc
      Token_type.module_
      (Token_modifiers_set.singleton Declaration);
    self.module_type self pmd_type;
    self.attributes self pmd_attributes

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

  let signature_item (self : Ast_iterator.iterator)
      (si : Parsetree.signature_item) =
    match
      (* TODO *)
      match si.psig_desc with
      | Psig_typext _ | Psig_typesubst _
      | Psig_type (_, _)
      | Psig_value _
      | Psig_modtype _
      | Psig_exception _
      | Psig_module _
      | Psig_modsubst _
      | Psig_recmodule _
      | Psig_modtypesubst _
      | Psig_open _
      | Psig_include _
      | Psig_class _
      | Psig_class_type _
      | Psig_attribute _
      | Psig_extension (_, _) -> `Default_iterator
    with
    | `Default_iterator -> Ast_iterator.default_iterator.signature_item self si
    | `Custom_iterator -> ()

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
    ; signature_item
    ; module_type_declaration
    ; attribute
    ; attributes
    ; module_expr
    ; value_description
    ; module_type
    ; module_declaration
    }

  let apply parsetree =
    (match parsetree with
    | `Interface signature -> iterator.signature iterator signature
    | `Implementation structure -> iterator.structure iterator structure);
    tokens
end

(* Note: this is global mutable state; we could move this to [State.t] *)
let gen_new_id =
  let i = ref 0 in
  fun () ->
    let x = !i in
    incr i;
    string_of_int x

let compute_tokens doc =
  let+ parsetree = Document.with_pipeline_exn doc Mpipeline.reader_parsetree in
  let module Fold = Parsetree_fold () in
  Fold.apply parsetree

let compute_encoded_tokens doc =
  let+ tokens = compute_tokens doc in
  Tokens.encode tokens

module Debug = struct
  let meth_request_full = "ocamllsp/textDocument/semanticTokens/full"

  let on_request_full :
      params:Jsonrpc.Structured.t option -> State.t -> Json.t Fiber.t =
   fun ~params state ->
    Fiber.of_thunk (fun () ->
        match params with
        | None ->
          Jsonrpc.Response.Error.raise
          @@ Jsonrpc.Response.Error.make
               ~code:Jsonrpc.Response.Error.Code.InvalidParams
               ~message:
                 (meth_request_full
                ^ " expects an argument but didn't receive any")
               ()
        | Some (`Assoc _ as json) | Some (`List _ as json) ->
          let params = SemanticTokensParams.t_of_yojson json in
          let store = state.store in
          let uri = params.textDocument.uri in
          let doc = Document_store.get store uri in
          let+ tokens = compute_tokens doc in
          Tokens.yojson_of_t tokens)
end

let on_request_full :
    State.t -> SemanticTokensParams.t -> SemanticTokens.t option Fiber.t =
 fun state params ->
  Fiber.of_thunk (fun () ->
      let store = state.store in
      let uri = params.textDocument.uri in
      let doc = Document_store.get store uri in
      let+ tokens = compute_encoded_tokens doc in
      let resultId = gen_new_id () in
      Document_store.update_semantic_tokens_cache store uri ~resultId ~tokens;
      Some { SemanticTokens.resultId = Some resultId; data = tokens })

(* TODO: refactor [find_diff] and write (inline?) tests *)

(* [find_diff] finds common prefix and common suffix and reports the rest as
   array difference. This is not ideal but good enough. The idea comes from the
   Rust Analyzer implementation of this function. *)
let find_diff ~(old : int array) ~(new_ : int array) : SemanticTokensEdit.t list
    =
  let old_len = Array.length old in
  let new_len = Array.length new_ in
  let left_offset = Array.common_prefix_len ~equal:Int.equal old new_ in
  if left_offset = old_len then
    if left_offset = new_len then (* [old] and [new_] are simply equal *) []
    else
      (* [old] is prefix of [new_] *)
      [ SemanticTokensEdit.create
          ~start:left_offset
          ~deleteCount:0
          ~data:(Array.sub new_ ~pos:left_offset ~len:(new_len - left_offset))
          ()
      ]
  else if left_offset = new_len then
    (* [new_] is prefix of [old] *)
    [ SemanticTokensEdit.create
        ~start:left_offset
        ~deleteCount:(old_len - left_offset)
        ()
    ]
  else
    let old_noncommon = Array.View.make old ~pos:left_offset () in
    let new_noncommon = Array.View.make new_ ~pos:left_offset () in
    let common_suffix_len =
      Array.View.common_suffix_len old_noncommon new_noncommon
    in
    let right_offset_old = old_len - common_suffix_len in
    let right_offset_new = new_len - common_suffix_len in
    let data =
      Array.sub new_ ~pos:left_offset ~len:(right_offset_new - left_offset)
    in
    [ SemanticTokensEdit.create
        ~start:left_offset
        ~deleteCount:(right_offset_old - left_offset)
        ~data
        ()
    ]

let on_request_full_delta :
       State.t
    -> SemanticTokensDeltaParams.t
    -> [ `SemanticTokens of SemanticTokens.t
       | `SemanticTokensDelta of SemanticTokensDelta.t
       ]
       option
       Fiber.t =
 fun state params ->
  Fiber.of_thunk (fun () ->
      let store = state.store in
      let uri = params.textDocument.uri in
      let doc = Document_store.get store uri in
      let+ tokens = compute_encoded_tokens doc in
      let resultId = gen_new_id () in
      let cached_token_info =
        Document_store.get_semantic_tokens_cache
          state.store
          params.textDocument.uri
      in
      Document_store.update_semantic_tokens_cache store uri ~resultId ~tokens;
      match cached_token_info with
      | Some cached_v
        when String.equal cached_v.resultId params.previousResultId ->
        let edits = find_diff ~old:cached_v.tokens ~new_:tokens in
        Some
          (`SemanticTokensDelta
            { SemanticTokensDelta.resultId = Some resultId; edits })
      | Some _ | None ->
        Some
          (`SemanticTokens
            { SemanticTokens.resultId = Some resultId; data = tokens }))
