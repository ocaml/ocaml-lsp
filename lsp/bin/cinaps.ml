open Import

let preprocess_metamodel =
  object (self)
    inherit Metamodel.map as super

    method! or_ path (types : Metamodel.type_ list) =
      match
        List.filter_map types ~f:(function
          | Metamodel.Literal (Record []) -> None
          | type_ -> Some (self#type_ path type_))
      with
      | [] -> assert false
      | [ type_ ] -> type_
      | types -> super#or_ path types

    method! enumeration m =
      if m.name <> "TraceValue"
      then super#enumeration m
      else (
        (* ["compact"] is not part of the LSP specification. It leaked into an older
           upstream meta-model from VS Code's implementation-specific trace mode, and
           previous OCaml-LSP releases consequently accepted it. Keep accepting it for
           backwards compatibility. *)
        let compact : Metamodel.enumerationEntry =
          { name = "Compact"
          ; value = `String "compact"
          ; doc = { since = None; documentation = None }
          }
        in
        super#enumeration { m with values = compact :: m.values })
  end
;;

let expand_superclasses db (m : Metamodel.t) =
  let structures =
    let uniquify_fields fields =
      List.fold_left fields ~init:String.Map.empty ~f:(fun acc (f : Metamodel.property) ->
        String.Map.add acc ~key:f.name ~data:f)
      |> String.Map.bindings
      |> List.map ~f:snd
    in
    let rec fields_of_type (t : Metamodel.type_) =
      match t with
      | Reference s ->
        (match Metamodel.Entity.DB.find db s with
         | Structure s -> fields_of_structure s
         | Enumeration _ -> assert false
         | Alias a -> fields_of_type a.type_)
      | _ -> assert false
    and fields_of_structure (s : Metamodel.structure) =
      let fields = List.map (s.extends @ s.mixins) ~f:fields_of_type @ [ s.properties ] in
      List.concat fields
    in
    List.map m.structures ~f:(fun s ->
      let properties = fields_of_structure s |> uniquify_fields in
      { s with properties })
  in
  { m with structures }
;;

let ocaml =
  lazy
    (Metamodel_lsp.t ()
     |> preprocess_metamodel#t
     |> (fun metamodel ->
     let db = Metamodel.Entity.DB.create metamodel in
     expand_superclasses db metamodel)
     |> Typescript.of_metamodel
     |> Ocaml.of_typescript)
;;

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
  ;;

  let _skip (t : t) name =
    match t.modules with
    | [] -> failwith "non left to skip"
    | m :: modules ->
      let name' = module_name t m in
      assert (String.equal name name');
      t.modules <- modules
  ;;

  let pp_file pp ch =
    let fmt = Format.formatter_of_out_channel ch in
    Pp.to_fmt fmt pp;
    Format.pp_print_flush fmt ()
  ;;

  let write t cmd =
    let to_write, modules =
      match cmd with
      | `Finish -> t.modules, []
      | `Until m ->
        let rec loop xs acc =
          match xs with
          | [] -> List.rev acc, []
          | x :: xs ->
            if module_name t x = m then List.rev acc, x :: xs else loop xs (x :: acc)
        in
        loop t.modules []
    in
    t.modules <- modules;
    List.iter to_write ~f:(fun m ->
      let pp = Module.pp m in
      let pp = Ml.Kind.Map.get pp t.kind in
      pp_file pp t.out)
  ;;
end

let print_ml () =
  let output = Output.create (Lazy.force ocaml) Ml.Kind.Impl stdout in
  Output.write output `Finish
;;

let print_mli () =
  let output = Output.create (Lazy.force ocaml) Ml.Kind.Intf stdout in
  Output.write output `Finish
;;
