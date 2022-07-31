open Import

let ocaml =
  lazy
    (let lexing = Lexing.from_string Spec._3_16 in
     let typescript = Markdown.read_typescript lexing in
     let asts = Typescript.of_snippets typescript in
     Ocaml.of_typescript asts)

module Output = struct
  open Ocaml

  type t =
    { mutable modules : Module.t list
    ; kind : Ml.Kind.t
    ; out : out_channel
    }

  let create modules kind out_channel = { modules; out = out_channel; kind }

  let module_name (t : t) (m : Module.t) =
    match t.kind with
    | Intf -> (m.intf.name :> string)
    | Impl -> (m.impl.name :> string)

  let _skip (t : t) name =
    match t.modules with
    | [] -> failwith "non left to skip"
    | m :: modules ->
      let name' = module_name t m in
      assert (String.equal name name');
      t.modules <- modules

  let pp_file pp ch =
    let fmt = Format.formatter_of_out_channel ch in
    Pp.to_fmt fmt pp;
    Format.pp_print_flush fmt ()

  let write t cmd =
    let to_write, modules =
      match cmd with
      | `Finish -> (t.modules, [])
      | `Until m ->
        let rec loop xs acc =
          match xs with
          | [] -> (List.rev acc, [])
          | x :: xs ->
            if module_name t x = m then (List.rev acc, x :: xs)
            else loop xs (x :: acc)
        in
        loop t.modules []
    in
    t.modules <- modules;
    List.iter to_write ~f:(fun m ->
        let pp = Module.pp m in
        let pp = Ml.Kind.Map.get pp t.kind in
        pp_file pp t.out)
end

let print_ml () =
  let output = Output.create (Lazy.force ocaml) Ml.Kind.Impl stdout in
  Output.write output `Finish

let print_mli () =
  let output = Output.create (Lazy.force ocaml) Ml.Kind.Intf stdout in
  Output.write output `Finish
