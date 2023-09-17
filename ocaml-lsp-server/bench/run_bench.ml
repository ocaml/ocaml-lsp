open Core
open Core_bench
open Ocaml_lsp_server.Testing

let () =
  let document =
    "let mem = ListLabels.mem\n\nlet _ = mem ~se" |> Merlin_kernel.Msource.make
  in

  let position = `Logical (3, 15) in
  Command.summary
    (Bench.make_command
       [ Bench.Test.create ~name:"non-regex" (fun _ ->
             Compl.prefix_of_position ~short_path:false document position
             |> ignore)
       ; Bench.Test.create ~name:"regex" (fun _ ->
             Compl.prefix_of_position_regex ~short_path:false document position
             |> ignore)
       ; Bench.Test.create ~name:"old" (fun _ ->
             Compl.prefix_of_position_old ~short_path:false document position
             |> ignore)
       ])
  |> ignore
