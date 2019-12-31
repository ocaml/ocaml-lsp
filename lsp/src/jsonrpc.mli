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
      }
  end

  type t =
    { id : Id.t
    ; jsonrpc : string
    ; result : (json, Error.t) Result.t
    }

  val make : Id.t -> json -> t

  val make_error : Id.t -> int -> string -> t

  include Yojsonable.S with type t := t
end
