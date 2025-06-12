open Import

val run
  :  log_info:Lsp_timing_logger.t
  -> ClientCapabilities.t
  -> Document.t
  -> Uri.t
  -> [> `DocumentSymbol of DocumentSymbol.t list
     | `SymbolInformation of SymbolInformation.t list
     ]
       option
       Fiber.t
