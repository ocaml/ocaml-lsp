open Import
open Pp.O
open Pp

type t = unit Pp.t

type w = t

(* This module contains all the writing primitives *)

let ident = verbatim

let i = verbatim

let quoted s = i (sprintf "%S" s)

let surround delim a =
  let start, finish =
    match delim with
    | `Paren -> (i "(", i ")")
    | `Curly -> (i "{", i "}")
    | `Square -> (i "[", i "]")
  in
  Pp.concat [ start; a; finish ]

module Json = struct
  let invalid_pat name =
    (ident "json", Pp.textf "Json.error \"invalid %s\" json" name)

  let typ = "Json.t"

  module Literal = struct
    let str n = sprintf "`String %S" n

    let int i = sprintf "`Int (%d)" i

    let null = "`Null"

    let bool b = sprintf "`Bool %b" b
  end

  let str = sprintf "`String %s"

  let int = sprintf "`Int %s"

  let bool = sprintf "`Bool %s"
end

module Gen = struct
  let record ~delim fields =
    let sep = Pp.concat [ Pp.verbatim ";"; Pp.newline ] in
    Pp.text "{"
    ++ Pp.concat_map ~sep fields ~f:(fun (name, f) ->
           Pp.concat [ Pp.textf "%s %s " name delim; f ])
    ++ Pp.verbatim "}"

  let clause ~delim l r = Pp.concat [ l; Pp.verbatim (sprintf " %s " delim); r ]
end

module Attr = struct
  type t =
    { name : string
    ; payload : w list
    }

  let make name payload = { name; payload }

  let pp kind { name; payload } =
    let kind =
      match kind with
      | `Field -> "@"
      | `Type -> "@@"
    in
    Pp.concat [ i kind; i name; Pp.space; Pp.concat ~sep:Pp.space payload ]
    |> surround `Square
end

module Type = struct
  let string = i "string"

  let int = i "int"

  let name = i

  let bool = i "bool"

  let gen_decl kw name body =
    Pp.concat [ Pp.textf "%s %s =" kw name; Pp.newline; body ]

  let and_ name body = gen_decl "and" name body

  let decl name body = gen_decl "type" name body

  let record fields = Gen.record ~delim:":" fields

  let field_attrs ~field ~attrs =
    match attrs with
    | [] -> field
    | attrs ->
      let attrs = Pp.concat_map attrs ~sep:Pp.space ~f:(Attr.pp `Field) in
      Pp.concat [ field; Pp.space; attrs ]

  let var typ = Pp.textf "'%s" typ

  let app typ = function
    | [] -> assert false
    | [ x ] -> Pp.concat [ x; Pp.space; typ ]
    | xs ->
      let args =
        let sep = Pp.verbatim "," in
        Pp.concat [ Pp.verbatim "("; Pp.concat ~sep xs; Pp.verbatim ")" ]
      in
      Pp.concat [ args; Pp.space; typ ]

  let tuple fields =
    let sep = i "*" in
    i "(" ++ Pp.concat ~sep fields ++ i ")"

  let rec_decls xs =
    match xs with
    | [] -> Pp.concat []
    | (name, body) :: xs ->
      decl name body ++ newline
      ++ Pp.concat_map xs ~sep:Pp.newline ~f:(fun (name, body) ->
             and_ name body)

  let deriving td ~record =
    let fields =
      if record then
        space ++ i "[@@yojson.allow_extra_fields]"
      else
        space
    in
    Pp.concat
      [ td
      ; Pp.newline
      ; Pp.text "[@@deriving_inline yojson]"
      ; fields
      ; space
      ; Pp.text "[@@@end]"
      ]

  let opt_attr = ident "option [@yojson.option]"

  let opt_field f = Pp.seq f opt_attr

  let default f def = Pp.concat [ f; ident "[@default "; ident def; ident "]" ]

  let key name = concat [ ident "[@key "; quoted name; ident "]" ]

  let gen_variant ~poly constrs =
    let sep = Pp.concat [ Pp.newline; i "| " ] in
    Pp.concat_map constrs ~sep ~f:(fun (name, arg) ->
        let name =
          let name = String.capitalize_ascii name in
          if poly then
            "`" ^ name
          else
            name
        in
        match arg with
        | [] -> i name
        | xs ->
          let xs =
            match xs with
            | [ x ] -> x
            | xs -> tuple xs
          in
          Gen.clause ~delim:"of" (ident name) xs)

  let poly constrs = concat [ i "["; gen_variant ~poly:true constrs; i "]" ]

  let variant constrs = gen_variant ~poly:false constrs
end

let gen_module kw name body =
  Pp.concat
    [ Pp.textf "module %s %s" name kw
    ; Pp.newline
    ; body
    ; newline
    ; verbatim "end"
    ; newline
    ]

module Sig = struct
  let module_ name body = gen_module ": sig" name body

  let include_ name destructive_subs =
    let inc_ = Pp.textf "include %s" name in
    match destructive_subs with
    | [] -> inc_
    | substs ->
      let substs =
        let sep = Pp.text " and " in
        Pp.concat_map ~sep substs ~f:(fun (l, r) ->
            Pp.concat
              [ Pp.text "type"
              ; Pp.space
              ; l
              ; Pp.space
              ; Pp.verbatim ":="
              ; Pp.space
              ; r
              ])
      in
      Pp.concat [ inc_; Pp.space; Pp.text "with"; Pp.space; substs ]

  let val_ name b =
    let sep = Pp.concat [ space; i "->"; space ] in
    let b = Pp.concat ~sep b in
    Pp.concat [ textf "val %s : " name; b; Pp.newline ]

  let assoc k v = Pp.concat [ Type.tuple [ k; v ]; Pp.space; i "list" ]
end

let warnings codes = seq (textf "[@@@warning %S]" codes) newline

let opens names =
  Pp.concat_map names ~f:(fun name ->
      Pp.concat [ textf "open! %s" name; newline ])

let module_ name body = gen_module "= struct" name body

let record fields = Gen.record ~delim:"=" fields
