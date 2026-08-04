open Import
open Types

(** Helpers for the semantic-tokens wire format: legend negotiation, absolute
    token encoding, and full/delta edits. *)

module Legend : sig
  type t

  val create : token_types:string list -> token_modifiers:string list -> t
  val token_types : t -> string list
  val token_modifiers : t -> string list
  val to_types : t -> SemanticTokensLegend.t
end

module Encoding : sig
  (** Negotiated mapping from a server legend to a client-supported legend.

      The advertised legend preserves server order and keeps only values the
      client supports (by name membership). Client order and duplicates do not
      affect the result. *)
  type t

  val negotiate : server:Legend.t -> client:Legend.t -> t

  (** Legend the server should advertise after negotiation. *)
  val legend : t -> Legend.t

  (** Map a server token-type index to the negotiated legend index, or [None]
      when the client does not support that type. *)
  val token_type_index : t -> server_index:int -> int option

  (** Remap a server-side modifier bitset into the negotiated legend. *)
  val token_modifiers_bitset : t -> server_bitset:int -> int
end

module Token : sig
  (** Absolute source token already mapped to negotiated legend indexes. *)
  type t =
    { start : Position.t
    ; length : int
    ; token_type : int
    ; token_modifiers : int
    }
end

(** Encode absolute tokens as an LSP semantic-tokens data array.

    Tokens may be provided in any order; they are sorted by source position
    before delta encoding. [length] must be positive. *)
val encode : Token.t list -> int array

val find_diff : old:int array -> new_:int array -> SemanticTokensEdit.t list
