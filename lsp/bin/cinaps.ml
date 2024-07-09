open Import

let preprocess_metamodel =
  object (self)
    inherit Metamodel.map as super

    method! or_ path (types : Metamodel.type_ list) =
      match
        List.filter_map types ~f:(function
          | Literal (Record []) -> None
          | _ as t -> Some (self#type_ path t))
      with
      | [] -> assert false
      | [ t ] -> t
      | [ Metamodel.Literal (Record f1); Literal (Record f2) ] as ts ->
        (match path with
         | Top (Alias s) when s.name = "TextDocumentContentChangeEvent" ->
           let t =
             let union_fields l1 l2 ~f =
               let of_map =
                 String.Map.of_list_map_exn ~f:(fun (x : Metamodel.property) -> x.name, x)
               in
               String.Map.merge (of_map l1) (of_map l2) ~f |> String.Map.values
             in
             union_fields f1 f2 ~f:(fun k t1 t2 ->
               if k = "text"
               then t1
               else if k = "range"
               then (
                 match t1, t2 with
                 | None, Some s | Some s, None ->
                   assert (not s.optional);
                   Some { s with optional = true }
                 | None, None | Some _, Some _ -> assert false)
               else (
                 match t1, t2 with
                 | None, None -> assert false
                 | Some s, None | None, Some s -> Some s
                 | Some _, Some _ -> assert false))
           in
           self#type_ path (Metamodel.Literal (Record t))
         | _ -> super#or_ path ts)
      | ts -> super#or_ path ts

    method! property path (p : Metamodel.property) =
      let update_type type_ =
        let type_ = self#type_ path type_ in
        super#property path { p with type_ }
      in
      let open Metamodel.Path in
      match path with
      | Top (Structure s)
        when p.name = "trace"
             && (s.name = "_InitializeParams" || s.name = "InitializeParams") ->
        update_type (Reference "TraceValues")
      | Top (Structure s) when p.name = "location" && s.name = "WorkspaceSymbol" ->
        (match p.type_ with
         | Or [ type_; _ ] -> update_type type_
         | _ -> assert false)
      | _ -> super#property path p

    method! enumeration m =
      match m.name = "TraceValues" with
      | false -> super#enumeration m
      | true ->
        super#enumeration
          (let values =
             let compact : Metamodel.enumerationEntry =
               { name = "Compact"
               ; value = `String "compact"
               ; doc = { since = None; documentation = None }
               }
             in
             compact :: m.values
           in
           { m with values })
  end
;;

let expand_superclasses db (m : Metamodel.t) =
  let structures =
    let uniquify_fields fields =
      List.fold_left fields ~init:String.Map.empty ~f:(fun acc (f : Metamodel.property) ->
        String.Map.set acc f.name f)
      |> String.Map.values
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
