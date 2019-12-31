open Import

module Request : sig
  type t =
    { id : Protocol.Id.t option [@default None]
    ; method_ : string [@key "method"]
    ; params : json
    }

  include Yojsonable.S with type t := t
end

module Response : sig
  type response =
    { id : Protocol.Id.t
    ; jsonrpc : string
    ; result : json
    }

  type error =
    { code : int
    ; message : string
    }

  type response_error =
    { id : Protocol.Id.t
    ; jsonrpc : string
    ; error : error
    }

  type t =
    | Response of response
    | Response_error of response_error

  val make : Protocol.Id.t -> json -> t

  val make_error : Protocol.Id.t -> int -> string -> t

  include Yojsonable.S with type t := t
end
