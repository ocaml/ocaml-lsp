open Import
open Types

(** Helpers for the [textDocument/documentSymbol] request.

    The hierarchical [DocumentSymbol] representation is the protocol's native
    shape, but clients without [hierarchicalDocumentSymbolSupport] require the
    flat [SymbolInformation] representation instead. *)

(** [flatten ~uri ?containerName symbols] converts a hierarchical symbol tree
    into a flat [SymbolInformation] list in depth-first order. Each symbol's
    name becomes the [containerName] of its children; [deprecated] and [tags]
    are preserved. *)
val flatten
  :  uri:DocumentUri.t
  -> ?containerName:string
  -> DocumentSymbol.t list
  -> SymbolInformation.t list
