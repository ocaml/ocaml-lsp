open Import

val normalize_selection_range : range:Range.t -> Range.t option -> Range.t

val run
  :  ClientCapabilities.t
  -> Document.t
  -> Uri.t
  -> [> `DocumentSymbol of DocumentSymbol.t list
     | `SymbolInformation of SymbolInformation.t list
     ]
       option
       Fiber.t
