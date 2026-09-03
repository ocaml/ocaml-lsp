val run : Lsp.Cli.Channel.t -> prefer_dot_merlin:bool -> unit -> unit

module Diagnostics = Diagnostics
module Version = Version
module Position = Position
module Doc_to_md = Doc_to_md

module For_tests : sig
  module Dune : module type of Dune.For_tests
end

module Testing = Testing
module Custom_request = Custom_request
