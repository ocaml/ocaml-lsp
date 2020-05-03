open Import

module Id : sig
  type t = (string, int) Either.t

  include Json.Jsonable.S with type t := t
end

module Request : sig
  type t =
    { id : Id.t option
    ; method_ : string
    ; params : Json.t option
    }

  include Json.Jsonable.S with type t := t

  val params : t -> (Json.t -> 'a) -> ('a, string) Result.t

  val create : ?id:Id.t -> ?params:Json.t -> method_:string -> unit -> t
end

module Response : sig
  module Error : sig
    module Code : sig
      type t =
        | ParseError
        | InvalidRequest
        | MethodNotFound
        | InvalidParams
        | InternalError
        | ServerErrorStart
        | ServerErrorEnd
        | ServerNotInitialized
        | UnknownErrorCode
        | RequestCancelled
        | ContentModified
    end

    type t =
      { code : Code.t
      ; message : string
      ; data : Json.t option
      }

    exception E of t

    val make : ?data:Json.t -> code:Code.t -> message:string -> unit -> t

    val raise : t -> 'a

    val of_exn : Exn.t -> t
  end

  type t =
    { id : Id.t
    ; result : (Json.t, Error.t) Result.t
    }

  val ok : Id.t -> Json.t -> t

  val error : Id.t -> Error.t -> t

  include Json.Jsonable.S with type t := t
end
