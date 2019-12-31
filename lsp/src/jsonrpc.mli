open Import

module Id : sig
  type t = (string, int) Either.t

  include Yojsonable.S with type t := t
end

module Request : sig
  type t =
    { id : Id.t option [@default None]
    ; method_ : string [@key "method"]
    ; params : json
    }

  include Yojsonable.S with type t := t
end

module Response : sig
  module Error : sig
    type t =
      { code : int
      ; message : string
      ; data : json option
      }

    val make : ?data:json -> code:int -> message:string -> unit -> t
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
