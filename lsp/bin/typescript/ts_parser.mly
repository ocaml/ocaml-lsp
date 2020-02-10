%{
  open Ts_types
  open! Import
%}

%token Const
%token Enum
%token Extends
%token Export
%token Interface
%token Namespace
%token Readonly
%token Type
%token L_curly R_curly
%token L_square R_square
%token L_angle R_angle
%token R_paren L_paren
%token Semicolon
%token Comma
%token<string> Ident
%token Colon
%token Alt
%token <float> Float
%token <int> Int
%token <string> String
%token Array_type
%token Question
%token Equal
%token Eof

%type <Type.Literal.t> lit
%type <Type.t> typ

%start <Ts_types.t list> main

%%

/* let field_name := */
/*   | ident = Ident; { `Required ident } */
/*   | ident = Ident; Question; { `Optional ident } */

let field_name :=
  | i = Ident; { i }
  | Type; { "type" }

let field :=
  | Readonly?; name = field_name; q = Question?; Colon; t = toplevel_typ; Semicolon?;
    { let optional = Option.is_some q in
      Field.named ~optional t name }
  | L_square
    ; name = field_name; Colon; pat = toplevel_typ
    ; R_square; Colon; typ = toplevel_typ ; Semicolon?;
    { Field.pattern ~name ~pat ~typ
    }
  /* | fn = field_name; Colon; t = typ; Semicolon; { */
  /*   let (optional, name) = */
  /*     match fn with */
  /*     | `Required name -> (false, name) */
  /*     | `Optional name -> (true, name) */
  /*   in */
  /*   Field.named ~optional t name */
  /* } */

let lit :=
  | lit = String; { Type.Literal.String lit }
  | lit = Int; { Type.Literal.Int lit }
  | lit = Float; { Type.Literal.Float lit }

let fields :=
  | L_curly
    ; fields = list(field)
    ; Semicolon?
    ; R_curly ; { fields }

let typ :=
  | l = lit; { Type.Literal l }
  | ident = Ident; {
      match Type.Prim.of_string ident with
      | Some s -> Prim s
      | None -> Name ident
    }
  | sum = delimited(L_paren, separated_nonempty_list(Alt, typ), R_paren);
    { Sum sum }
  /* | types = separated_nonempty_list(Alt, typ); { Sum types } */
  | t = typ ; Array_type; { Type.List t }
  | ~ = fields; { Type.Record fields }
  | t = typ ; a = delimited (L_angle, typ, R_angle); { Type.App (t, a) }
  | typs = delimited(L_square, separated_nonempty_list(Comma, typ), R_square);
    { Tuple typs }
  /* | delimited(L_paren, typ, R_paren) */

let toplevel_typ ==
  | types = separated_nonempty_list(Alt, typ); { Type.Sum types }
  | ~ = typ; { typ }

let extends := Extends; separated_list(Comma, Ident)

let params := idents = delimited(L_angle, separated_list(Comma, Ident), R_angle); { idents }

let interface :=
  | Export?; Interface; name = Ident
    ; params = params?
    ; extends = extends?
    ; ~ = fields;
    { let extends = match extends with None -> [] | Some xs -> xs in
      let params = match params with None -> [] | Some xs -> xs in
      Interface.make ~name ~extends ~fields ~params
    }

let const_constr :=
  | Export; Const; name = Ident; Equal; ~ = lit; Semicolon;
    { (name, lit)
    }
  | Export; Const; name = Ident; Colon; Ident; Equal; ~ = lit; Semicolon;
    { (name, lit)
    }
  | Export; Const; name = Ident; Colon; lit; Equal; ~ = lit; Semicolon;
    { (name, lit)
    }

let enum_constr := name = Ident; Equal; v = lit; { (name, v) }
let enum_constrs := separated_nonempty_list(Comma, enum_constr)

let enum :=
  | Enum; name = Ident
    ; L_curly; constrs = enum_constrs ; R_curly;
    { Enum.named ~name ~constrs
    }
  | Export?; Namespace; name = Ident ; L_curly
    ; constrs = list(const_constr)
    ; R_curly ;
    { Enum.named ~name ~constrs
    }

let alias :=
  | Type; name = Ident; Equal; typ = toplevel_typ; Semicolon;
    { Named.make ~name typ }

let type_decl :=
  | Export; Type; name = Ident; Equal; typ = toplevel_typ; Semicolon?;
    { Named.make ~name typ }

let definition :=
  | ~ = interface; { Interface interface }
  | ~ = enum; { Enum enum }
  | ~ = type_decl; { Type type_decl }
  | ~ = alias; { Alias alias }

let main :=
  | defs = definition*; Eof; { defs }
