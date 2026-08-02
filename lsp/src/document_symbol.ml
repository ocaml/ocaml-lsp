open Import

open struct
  open Types
  module DocumentSymbol = DocumentSymbol
  module SymbolInformation = SymbolInformation
  module Location = Location
end

let rec flatten ~uri ?containerName (symbols : DocumentSymbol.t list) =
  List.concat_map symbols ~f:(fun (symbol : DocumentSymbol.t) ->
    let symbol_information =
      SymbolInformation.create
        ?containerName
        ~kind:symbol.kind
        ~location:{ Location.range = symbol.range; uri }
        ~name:symbol.name
        ?deprecated:symbol.deprecated
        ?tags:symbol.tags
        ()
    in
    let children =
      Option.value symbol.children ~default:[]
      |> flatten ~uri ?containerName:(Some symbol.name)
    in
    symbol_information :: children)
;;
