open Import

let () =
  let md_file = ref None in
  let out_dir = ref None in
  let args =
    [ "--md"
    , Arg.String (fun s -> md_file := Some s)
    , "markdown file containing specification"
    ; "--out-dir"
    , Arg.String (fun s -> out_dir := Some s)
    , "output directory"
    ]
  in
  let anon s = raise (Arg.Bad (sprintf "don't know what to do with %s" s)) in
  let usage = sprintf "%s --md [FILE] --out-dor [DIR]"
      (Filename.basename Sys.executable_name) in
  Arg.parse args anon usage;
  let md_file = Option.value_exn !md_file in
  let out_dir = !out_dir in
  let ch = open_in md_file in
  let lexing = Lexing.from_channel ch in
  let typescript = Markdown.read_typescript lexing in
  let asts = Typescript.of_snippets typescript in
  let ocaml = Ocaml.of_typescript asts in
  match out_dir with
  | None -> print_endline "parsed successfully"
  | Some out_dir -> Ocaml.output ocaml out_dir
