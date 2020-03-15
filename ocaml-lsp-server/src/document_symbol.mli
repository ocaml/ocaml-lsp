open Import

val run :
     ClientCapabilities.t
  -> Document.t
  -> Lsp.Uri.t
  -> [> `DocumentSymbol of DocumentSymbol.t list
     | `SymbolInformation of SymbolInformation.t list
     ]
