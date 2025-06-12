open Import
open Json.Conv

module DebugEcho = struct
  module T = struct
    type t = { message : string } [@@deriving yojson]
  end

  module Params = T
  module Result = T
end

module DebugTextDocumentGet = struct
  module Params = Types.TextDocumentPositionParams

  module Result = struct
    type t = string option

    let yojson_of_t = function
      | None -> `Null
      | Some s -> `String s
    ;;

    let t_of_yojson = function
      | `Null -> None
      | `String s -> Some s
      | json -> Json.error "DebugTextDocumentGet" json
    ;;
  end
end
