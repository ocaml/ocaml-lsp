open Stdune

type doc =
  { since : string option
  ; documentation : string option
  }

type baseType =
  | Uri
  | DocumentUri
  | Integer
  | Uinteger
  | Decimal
  | RegExp
  | String
  | Boolean
  | Null

type mapKeyType =
  | Uri
  | DocumentUri
  | String
  | Integer
  | Reference of string

type literalType =
  | String of string
  | Boolean of bool
  | Integer of int
  | Record of property list

and property =
  { doc : doc
  ; name : string
  ; optional : bool
  ; type_ : type_
  }

and mapType =
  { key : mapKeyType
  ; value : type_
  }

and type_ =
  | Base of baseType
  | Reference of string
  | Array of type_
  | Or of type_ list
  | And of type_ list
  | Tuple of type_ list
  | Literal of literalType
  | Map of mapType

type typeAlias =
  { name : string
  ; type_ : type_
  ; doc : doc
  }

type enumerationEntry =
  { name : string
  ; value : [ `String of string | `Int of int ]
  ; doc : doc
  }

type enumerationType = { name : [ `String | `Integer | `Uinteger ] }

type enumeration =
  { doc : doc
  ; name : string
  ; supportsCustomValues : bool
  ; type_ : enumerationType
  ; values : enumerationEntry list
  }

type structure =
  { doc : doc
  ; extends : type_ list
  ; mixins : type_ list
  ; name : string
  ; properties : property list
  }

type call =
  { method_ : string
  ; params : [ `Param of type_ | `Params of type_ list ] option
  ; registrationOptions : type_ option
  ; doc : doc
  }

type request =
  { call : call
  ; errorData : type_ option
  ; partialResult : type_ option
  ; result : type_
  }

type notification = { call : call }

type t =
  { requests : request list
  ; notifications : notification list
  ; structures : structure list
  ; enumerations : enumeration list
  ; typeAliases : typeAlias list
  }

let error msg json =
  failwith (msg ^ "\n" ^ Yojson.Safe.pretty_to_string ~std:false json)

let fields = function
  | `Assoc xs -> xs
  | xs -> error "expected fields" xs

let field ?default (name : string) p fields =
  match List.assoc fields name with
  | Some f -> p f
  | None -> (
    match default with
    | None -> error ("field not found " ^ name) (`Assoc fields)
    | Some x -> x)

let field_o name p fields =
  match List.assoc fields name with
  | None -> None
  | Some f -> Some (p f)

let bool = function
  | `Bool b -> b
  | json -> error "boolean expected" json

let literal lit json =
  if not (Poly.equal json lit) then error "unexpected literal" json

let enum variants json =
  match json with
  | `String s -> (
    match List.assoc variants s with
    | None -> error "not a valid enum value" json
    | Some v -> v)
  | _ -> error "not a valid enum value" json

let string = function
  | `String s -> s
  | json -> error "expected string" json

let string_or_number = function
  | `String s -> `String s
  | `Int i -> `Int i
  | json -> error "expected string or number" json

let name fields = field "name" string fields

let list conv = function
  | `List xs -> List.map xs ~f:conv
  | json -> error "expected list" json

let baseType json : baseType =
  match json with
  | `String s -> (
    match s with
    | "Uri" -> Uri
    | "DocumentUri" -> DocumentUri
    | "integer" -> Integer
    | "uinteger" -> Uinteger
    | "decimal" -> Decimal
    | "RegExp" -> RegExp
    | "string" -> String
    | "boolean" -> Boolean
    | "null" -> Null
    | _ -> error "unknown base type" json)
  | _ -> error "unknown base type" json

let mapKeyType json : mapKeyType =
  let fields = fields json in
  let kind = field "kind" string fields in
  match kind with
  | "reference" -> Reference (name fields)
  | "base" ->
    field "name"
      (enum
         [ ("Uri", Uri)
         ; ("DocumentUri", DocumentUri)
         ; ("string", String)
         ; ("integer", Integer)
         ])
      fields
  | kind -> error ("invalid kind for map key type: " ^ kind) json

let doc fields =
  let since = field_o "since" string fields in
  let documentation = field_o "documentation" string fields in
  { since; documentation }

let rec type_ json =
  let fields_conv = fields in
  let fields = fields json in
  let kind = field "kind" string fields in
  match kind with
  | "reference" -> Reference (name fields)
  | "array" ->
    let element = field "element" type_ fields in
    Array element
  | "base" ->
    let b = field "name" baseType fields in
    Base b
  | "or" ->
    let items = field "items" (list type_) fields in
    Or items
  | "and" ->
    let items = field "items" (list type_) fields in
    And items
  | "tuple" ->
    let items = field "items" (list type_) fields in
    Tuple items
  | "stringLiteral" ->
    let value = field "value" string fields in
    Literal (String value)
  | "map" ->
    let key = field "key" mapKeyType fields in
    let value = field "value" type_ fields in
    Map { key; value }
  | "literal" ->
    let fields =
      field "value"
        (fun json ->
          let fields = fields_conv json in
          properties fields)
        fields
    in
    Literal (Record fields)
  | kind -> error "unrecognized kind" (`String kind)

and properties fields : property list =
  field "properties" (list property) fields

and property json : property =
  let fields = fields json in
  let name = name fields in
  let doc = doc fields in
  let type_ = type_field fields in
  let optional = field ~default:false "optional" bool fields in
  { name; type_; optional; doc }

and type_field fields = field "type" type_ fields

let params = function
  | `List l -> `Params (List.map l ~f:type_)
  | `Assoc _ as json -> `Param (type_ json)
  | json -> error "list or object expected" json

let call fields =
  let method_ = field "method" string fields in
  let params = field_o "params" params fields in
  let doc = doc fields in
  let registrationOptions = field_o "registrationOptions" type_ fields in
  { registrationOptions; doc; method_; params }

let notification json =
  let fields = fields json in
  let call = call fields in
  { call }

let request json =
  let fields = fields json in
  let call = call fields in
  let errorData = field_o "errorData" type_ fields in
  let partialResult = field_o "partialResult" type_ fields in
  let result = field "result" type_ fields in
  { call; errorData; partialResult; result }

let enumerationEntry json : enumerationEntry =
  let fields = fields json in
  let name = name fields in
  let doc = doc fields in
  let value = field "value" string_or_number fields in
  { name; value; doc }

let enumerationType json =
  let fields = fields json in
  let () = field "kind" (literal (`String "base")) fields in
  let name =
    field "name"
      (enum
         [ ("integer", `Integer); ("string", `String); ("uinteger", `Uinteger) ])
      fields
  in
  { name }

let enumeration json =
  let fields = fields json in
  let name = name fields in
  let doc = doc fields in
  let values = field "values" (list enumerationEntry) fields in
  let type_ = field "type" enumerationType fields in
  let supportsCustomValues =
    field ~default:false "supportsCustomValues" bool fields
  in
  { supportsCustomValues; type_; values; name; doc }

let structure json =
  let fields = fields json in
  let doc = doc fields in
  let name = name fields in
  let extends = field ~default:[] "extends" (list type_) fields in
  let mixins = field ~default:[] "mixins" (list type_) fields in
  let properties = properties fields in
  { doc; name; extends; mixins; properties }

let typeAlias json : typeAlias =
  let fields = fields json in
  let name = name fields in
  let type_ = type_field fields in
  let doc = doc fields in
  { doc; name; type_ }

let t json =
  let fields = fields json in
  let requests = field "requests" (list request) fields in
  let notifications = field "notifications" (list notification) fields in
  let structures = field "structures" (list structure) fields in
  let enumerations = field "enumerations" (list enumeration) fields in
  let typeAliases = field "typeAliases" (list typeAlias) fields in
  { requests; notifications; structures; enumerations; typeAliases }
