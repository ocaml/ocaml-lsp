open Ocaml_lsp_server
open Core
open Core_bench

let () =
  let open Documents in
  let long_document = long_document_text |> Merlin_kernel.Msource.make in
  let position = `Logical (3, 15) in
  let long_position = `Logical (92, 41) in
  Command_unix.run
    (Bench.make_command
       [ Bench.Test.create ~name:"get_prefix" (fun _ ->
           Testing.Compl.prefix_of_position ~short_path:false document position |> ignore)
       ; Bench.Test.create ~name:"get_prefix_long" (fun _ ->
           Testing.Compl.prefix_of_position ~short_path:false long_document long_position
           |> ignore)
       ; Bench.Test.create ~name:"get_offset_long" (fun _ ->
           Merlin_kernel.Msource.get_offset long_document long_position |> ignore)
       ])
;;
