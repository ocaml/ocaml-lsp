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
  ; value : [ `Int of int | `String of string ]
  ; doc : doc
  }

type enumerationType = { name : [ `Integer | `String | `Uinteger ] }

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

val t : Yojson.Safe.t -> t

module Entity : sig
  type metamodel := t

  type t =
    | Structure of structure
    | Enumeration of enumeration
    | Alias of typeAlias

  module DB : sig
    type entity := t
    type t

    val create : metamodel -> t
    val find : t -> string -> entity
  end
end

module Path : sig
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

class map : object
  method literal : Path.t -> literalType -> literalType
  method property : Path.t -> property -> property
  method or_ : Path.t -> type_ list -> type_
  method type_ : Path.t -> type_ -> type_
  method t : t -> t
  method request : request -> request
  method structure : structure -> structure
  method notification : notification -> notification
  method typeAlias : typeAlias -> typeAlias
  method enumeration : enumeration -> enumeration
end
