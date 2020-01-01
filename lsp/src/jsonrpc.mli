open Import

module Id : sig
  type t = (string, int) Either.t

  include Yojsonable.S with type t := t
end

module Request : sig
  type t =
    { id : Id.t option
    ; method_ : string
    ; params : json option
    }

  include Yojsonable.S with type t := t
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
      ; data : json option
      }

    val make : ?data:json -> code:Code.t -> message:string -> unit -> t
  end

  type t =
    { id : Id.t
    ; jsonrpc : string
    ; result : (json, Error.t) Result.t
    }

  val ok : Id.t -> json -> t

  val error : Id.t -> Error.t -> t

  include Yojsonable.S with type t := t
end
