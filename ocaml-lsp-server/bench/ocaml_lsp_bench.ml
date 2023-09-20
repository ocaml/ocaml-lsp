open Core
open Core_bench
open Ocaml_lsp_server.Testing

let () =
  let document =
    "let mem = ListLabels.mem\n\nlet _ = mem ~se" |> Merlin_kernel.Msource.make
  in
  let long_document_text =
    "pam .tsiL se~\n\
    \    arosietnaorisetnoarisent\n\
    \    arsotienarositen\n\
    \    arsotinarsotienarst\n\
    \    aoris noarisetnaoiresnt aorisent aoierns\n\
    \     ast. rienrst .rst \n\
    \    let b a= 5234 n oienar. rsoien . iri i... r\n\
    \    !@#&984@#$ <><|||>>\n\
    \    let mem = ListLabels.mem\n\n\
     let _ = mem ~se"
  in
  let long_document = long_document_text |> Merlin_kernel.Msource.make in

  let position = `Logical (3, 15) in
  Command_unix.run
    (Bench.make_command
       [ Bench.Test.create ~name:"regex" (fun _ ->
             Compl.prefix_of_position ~short_path:false document position
             |> ignore)
       ; Bench.Test.create ~name:"regex_long" (fun _ ->
             Compl.prefix_of_position ~short_path:false long_document position
             |> ignore)
       ; Bench.Test.create ~name:"regex_only" (fun _ ->
             Prefix_parser.try_parse_regex
               (document |> Merlin_kernel.Msource.text)
             |> ignore)
       ; Bench.Test.create ~name:"regex_only_long" (fun _ ->
             Prefix_parser.try_parse_regex long_document_text |> ignore)
       ; Bench.Test.create ~name:"old" (fun _ ->
             Compl.prefix_of_position_old ~short_path:false document position
             |> ignore)
       ; Bench.Test.create ~name:"old_long" (fun _ ->
             Compl.prefix_of_position_old ~short_path:false document position
             |> ignore)
       ])
