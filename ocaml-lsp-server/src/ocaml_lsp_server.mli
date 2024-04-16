val run :
     Lsp.Cli.Channel.t
  -> dune_context:string option
  -> read_dot_merlin:bool
  -> unit
  -> unit

module Diagnostics = Diagnostics
module Version = Version
module Position = Position
module Doc_to_md = Doc_to_md
module Testing = Testing
