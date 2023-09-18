open Core
open Core_bench
open Ocaml_lsp_server.Testing

let () =
  let document =
    "let mem = ListLabels.mem\n\nlet _ = mem ~se" |> Merlin_kernel.Msource.make
    in
  let long_document =
    "
    arosietnaorisetnoarisent
    arsotienarositen
    arsotinarsotienarst
     ast. rienrst .rst 
    !@#&984@#$ <><|||>>
    aoris noarisetnaoiresnt aorisent aoierns
    let b a= 5234 n oienar. rsoien . iri i... r
    let mem = ListLabels.mem\n\nlet _ = mem ~se" |> Merlin_kernel.Msource.make
  in

  let position = `Logical (3, 15) in
    Command_unix.run
    (Bench.make_command
       [ Bench.Test.create ~name:"regex" (fun _ ->
             Compl.prefix_of_position ~short_path:false document position
             |> ignore)
       ; Bench.Test.create ~name:"parser" (fun _ ->
             Compl.prefix_of_position_parser ~short_path:false document position
             |> ignore)
       ; Bench.Test.create ~name:"regex_long" (fun _ ->
             Compl.prefix_of_position ~short_path:false long_document position
             |> ignore)
       ; Bench.Test.create ~name:"old" (fun _ ->
             Compl.prefix_of_position_old ~short_path:false document position
             |> ignore)
       ])
