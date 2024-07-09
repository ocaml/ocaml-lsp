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

let error msg json = failwith (msg ^ "\n" ^ Yojson.Safe.pretty_to_string ~std:false json)

let fields = function
  | `Assoc xs -> xs
  | xs -> error "expected fields" xs
;;

let field ?default (name : string) p fields =
  match List.assoc fields name with
  | Some f -> p f
  | None ->
    (match default with
     | None -> error ("field not found " ^ name) (`Assoc fields)
     | Some x -> x)
;;

let field_o name p fields =
  match List.assoc fields name with
  | None -> None
  | Some f -> Some (p f)
;;

let bool = function
  | `Bool b -> b
  | json -> error "boolean expected" json
;;

let literal lit json = if not (Poly.equal json lit) then error "unexpected literal" json

let enum variants json =
  match json with
  | `String s ->
    (match List.assoc variants s with
     | None -> error "not a valid enum value" json
     | Some v -> v)
  | _ -> error "not a valid enum value" json
;;

let string = function
  | `String s -> s
  | json -> error "expected string" json
;;

let string_or_number = function
  | `String s -> `String s
  | `Int i -> `Int i
  | json -> error "expected string or number" json
;;

let name fields = field "name" string fields

let list conv = function
  | `List xs -> List.map xs ~f:conv
  | json -> error "expected list" json
;;

let baseType json : baseType =
  match json with
  | `String s ->
    (match s with
     | "URI" | "Uri" -> Uri
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
;;

let mapKeyType json : mapKeyType =
  let fields = fields json in
  let kind = field "kind" string fields in
  match kind with
  | "reference" -> Reference (name fields)
  | "base" ->
    field
      "name"
      (enum
         [ "Uri", Uri
         ; "URI", Uri
         ; "DocumentUri", DocumentUri
         ; "string", String
         ; "integer", Integer
         ])
      fields
  | kind -> error ("invalid kind for map key type: " ^ kind) json
;;

let doc fields =
  let since = field_o "since" string fields in
  let documentation = field_o "documentation" string fields in
  { since; documentation }
;;

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
      field
        "value"
        (fun json ->
          let fields = fields_conv json in
          properties fields)
        fields
    in
    Literal (Record fields)
  | kind -> error "unrecognized kind" (`String kind)

and properties fields : property list = field "properties" (list property) fields

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
;;

let call fields =
  let method_ = field "method" string fields in
  let params = field_o "params" params fields in
  let doc = doc fields in
  let registrationOptions = field_o "registrationOptions" type_ fields in
  { registrationOptions; doc; method_; params }
;;

let notification json =
  let fields = fields json in
  let call = call fields in
  { call }
;;

let request json =
  let fields = fields json in
  let call = call fields in
  let errorData = field_o "errorData" type_ fields in
  let partialResult = field_o "partialResult" type_ fields in
  let result = field "result" type_ fields in
  { call; errorData; partialResult; result }
;;

let enumerationEntry json : enumerationEntry =
  let fields = fields json in
  let name = name fields in
  let doc = doc fields in
  let value = field "value" string_or_number fields in
  { name; value; doc }
;;

let enumerationType json =
  let fields = fields json in
  let () = field "kind" (literal (`String "base")) fields in
  let name =
    field
      "name"
      (enum [ "integer", `Integer; "string", `String; "uinteger", `Uinteger ])
      fields
  in
  { name }
;;

let enumeration json =
  let fields = fields json in
  let name = name fields in
  let doc = doc fields in
  let values = field "values" (list enumerationEntry) fields in
  let type_ = field "type" enumerationType fields in
  let supportsCustomValues = field ~default:false "supportsCustomValues" bool fields in
  { supportsCustomValues; type_; values; name; doc }
;;

let structure json =
  let fields = fields json in
  let doc = doc fields in
  let name = name fields in
  let extends = field ~default:[] "extends" (list type_) fields in
  let mixins = field ~default:[] "mixins" (list type_) fields in
  let properties = properties fields in
  { doc; name; extends; mixins; properties }
;;

let typeAlias json : typeAlias =
  let fields = fields json in
  let name = name fields in
  let type_ = type_field fields in
  let doc = doc fields in
  { doc; name; type_ }
;;

let t json =
  let fields = fields json in
  let requests = field "requests" (list request) fields in
  let notifications = field "notifications" (list notification) fields in
  let structures = field "structures" (list structure) fields in
  let enumerations = field "enumerations" (list enumeration) fields in
  let typeAliases = field "typeAliases" (list typeAlias) fields in
  { requests; notifications; structures; enumerations; typeAliases }
;;

type metamodel = t

module Entity = struct
  type t =
    | Structure of structure
    | Enumeration of enumeration
    | Alias of typeAlias

  module DB = struct
    type nonrec t = t String.Map.t

    let create
      ({ structures; requests = _; notifications = _; enumerations; typeAliases } :
        metamodel)
      : t
      =
      let structures =
        String.Map.of_list_map_exn structures ~f:(fun s -> s.name, Structure s)
      in
      let enumerations =
        String.Map.of_list_map_exn enumerations ~f:(fun s -> s.name, Enumeration s)
      in
      let typeAliases =
        String.Map.of_list_map_exn typeAliases ~f:(fun a -> a.name, Alias a)
      in
      String.Map.union_exn structures enumerations |> String.Map.union_exn typeAliases
    ;;

    let find t x = String.Map.find_exn t x
  end
end

module Path = struct
  type top =
    | Request of request
    | Notification of notification
    | Structure of structure
    | Enumeration of enumeration
    | Alias of typeAlias

  type t =
    | Top of top
    | Property of property * t
end

class map =
  let open Path in
  object (self)
    method property path (p : property) =
      let path = Property (p, path) in
      { p with type_ = self#type_ path p.type_ }

    method literal path t =
      match (t : literalType) with
      | Record ps -> Record (List.map ps ~f:(self#property path))
      | _ -> t

    method or_ path types = Or (List.map types ~f:(self#type_ path))

    method type_ path t : type_ =
      match t with
      | Base _ as t -> t
      | Reference _ -> t
      | Array t -> Array (self#type_ path t)
      | Or types -> self#or_ path types
      | And ts -> And (List.map ts ~f:(self#type_ path))
      | Tuple ts -> Tuple (List.map ts ~f:(self#type_ path))
      | Literal lt -> Literal (self#literal path lt)
      | Map mt -> Map { mt with value = self#type_ path mt.value }

    method private call path (c : call) =
      let params =
        let params = function
          | `Param t -> `Param (self#type_ path t)
          | `Params ts -> `Params (List.map ts ~f:(self#type_ path))
        in
        Option.map ~f:params c.params
      in
      let registrationOptions = Option.map ~f:(self#type_ path) c.registrationOptions in
      { c with params; registrationOptions }

    method request (r : request) =
      let path = Top (Request r) in
      let call = self#call path r.call in
      let errorData = Option.map ~f:(self#type_ path) r.errorData in
      let partialResult = Option.map ~f:(self#type_ path) r.partialResult in
      let result = self#type_ path r.result in
      { call; errorData; partialResult; result }

    method notification { call } =
      let path = Top (Notification { call }) in
      { call = self#call path call }

    method structure s =
      let path = Top (Structure s) in
      let extends = List.map s.extends ~f:(self#type_ path) in
      let mixins = List.map s.mixins ~f:(self#type_ path) in
      let properties = List.map s.properties ~f:(self#property path) in
      { s with extends; mixins; properties }

    method typeAlias (a : typeAlias) =
      let path = Top (Alias a) in
      { a with type_ = self#type_ path a.type_ }

    method enumeration (e : enumeration) : enumeration = e

    method t { requests; notifications; structures; enumerations; typeAliases } =
      let requests = List.map requests ~f:self#request in
      let notifications = List.map notifications ~f:self#notification in
      let structures = List.map structures ~f:self#structure in
      let typeAliases = List.map typeAliases ~f:self#typeAlias in
      let enumerations = List.map enumerations ~f:self#enumeration in
      { enumerations; requests; notifications; structures; typeAliases }
  end
