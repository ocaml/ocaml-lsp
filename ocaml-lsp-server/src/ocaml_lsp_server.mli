open Async

val run : Lsp.Cli.Channel.t -> dot_merlin:string option -> unit Deferred.t

module Diagnostics = Diagnostics
module Version = Version
module Position = Position
module Doc_to_md = Doc_to_md
module Testing = Testing
module Custom_request = Custom_request
