open Import
open Pp.O
open Pp

(* This module contains all the writing primitives *)

let ident = verbatim

let i = verbatim

let quoted s = i (sprintf "%S" s)

module Name = struct
  let to_json t = sprintf "yojson_of_%s" t

  let of_json t = sprintf "%s_of_yojson" t
end

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

  let clause ~delim l r =
    Pp.concat
      [ verbatim "| "; l; Pp.verbatim (sprintf " %s " delim); r; Pp.newline ]
end

module Type = struct
  let gen_decl kw name body =
    Pp.concat [ Pp.textf "%s %s =" kw name; Pp.newline; body ]

  let and_ name body = gen_decl "and" name body

  let decl name body = gen_decl "type" name body

  let record fields = Gen.record ~delim:":" fields

  let rec_decls xs =
    match xs with
    | [] -> Pp.concat []
    | (name, body) :: xs ->
      decl name body ++ newline
      ++ Pp.concat_map xs ~sep:Pp.newline ~f:(fun (name, body) ->
             and_ name body)

  let deriving td =
    Pp.concat
      [ td
      ; Pp.newline
      ; Pp.text "[@@deriving_inline] [@@yojson.allow_extra_fields]"
      ; Pp.newline
      ; Pp.text "[@@@end]"
      ]

  let opt_attr = ident "option [@yojson.option]"

  let opt_field f = Pp.seq f opt_attr

  let default f def = Pp.concat [ f; ident "[@default "; ident def; ident "]" ]

  let key name = concat [ ident "[@key "; quoted name; ident "]" ]

  let variant_ constrs =
    Pp.concat_map constrs ~sep:Pp.newline ~f:(fun (name, arg) ->
        match arg with
        | None -> Pp.concat [ Pp.textf "| %s" name ]
        | Some a -> Gen.clause ~delim:"of" (ident name) a)
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

  let val_ name b =
    let sep = Pp.concat [ space; i "->"; space ] in
    let b = Pp.concat ~sep b in
    Pp.concat [ textf "val %s : " name; b; Pp.newline ]

  let tuple fields =
    let sep = i "," in
    i "(" ++ Pp.concat ~sep fields ++ i ")"

  let assoc k v = Pp.concat [ tuple [ k; v ]; Pp.space; i "list" ]

  module Json = struct
    let arr typ = [ i typ; i Json.typ ]

    let to_json typ = val_ (Name.to_json typ) (arr typ)

    let of_json typ = val_ (Name.of_json typ) (List.rev (arr typ))
  end
end

let warnings codes = seq (textf "[@@@warning %S]" codes) newline

let opens names =
  Pp.concat_map names ~f:(fun name ->
      Pp.concat [ textf "open! %s" name; newline ])

let module_ name body = gen_module "= struct" name body

let record fields = Gen.record ~delim:"=" fields

let match_ e clauses =
  let clauses =
    Pp.concat_map clauses ~f:(fun (l, r) -> Gen.clause ~delim:"->" l r)
  in
  Pp.concat [ Pp.textf "match %s with" e; Pp.newline; clauses ]

let to_yojson t body =
  Pp.concat
    [ Pp.textf "let %s (json : Json.t) : t =" (Name.to_json t)
    ; Pp.newline
    ; Pp.hovbox ~indent:2 body
    ]

let of_yojson t body =
  Pp.concat
    [ Pp.textf "let %s (t : t) : Json.t =" (Name.of_json t)
    ; Pp.newline
    ; Pp.hovbox ~indent:2 body
    ]
