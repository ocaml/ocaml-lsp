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
  type response =
    { id : Id.t
    ; jsonrpc : string
    ; result : json
    }

  type error =
    { code : int
    ; message : string
    }

  type response_error =
    { id : Id.t
    ; jsonrpc : string
    ; error : error
    }

  type t =
    | Response of response
    | Response_error of response_error

  val make : Id.t -> json -> t

  val make_error : Id.t -> int -> string -> t

  include Yojsonable.S with type t := t
end
