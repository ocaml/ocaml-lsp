open Import

val symbols_of_outline :
  Uri.t -> Query_protocol.item list -> SymbolInformation.t list

val run :
     ClientCapabilities.t
  -> Document.t
  -> Uri.t
  -> [> `DocumentSymbol of DocumentSymbol.t list
     | `SymbolInformation of SymbolInformation.t list
     ]
     Fiber.t
