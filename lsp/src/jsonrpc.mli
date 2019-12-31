open Import

module Request : sig
  type t =
    { id : Protocol.Id.t option [@default None]
    ; method_ : string [@key "method"]
    ; params : json
    }

  include Yojsonable.S with type t := t
end
