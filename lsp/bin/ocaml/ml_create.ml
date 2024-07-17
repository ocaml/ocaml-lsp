open Import

let f_name name = if name = "t" then "create" else sprintf "create_%s" name

let need_unit =
  List.exists ~f:(fun (f : Ml.Type.field) ->
    match f.typ with
    | Optional _ -> true
    | _ -> false)
;;

let intf { Named.name; data = fields } =
  let type_ =
    let need_unit = need_unit fields in
    let fields : Ml.Type.t Ml.Arg.t list =
      List.map fields ~f:(fun (field : Ml.Type.field) ->
        match field.typ with
        | Optional t -> Ml.Arg.Optional (field.name, t)
        | t -> Labeled (field.name, t))
    in
    let args : Ml.Type.t Ml.Arg.t list =
      if need_unit
      then
        (* Gross hack because I was too lazy to allow patterns in toplevel
           exprs *)
        fields @ [ Ml.Arg.Unnamed Ml.Type.unit ]
      else fields
    in
    Ml.Type.fun_ args (Ml.Type.name name)
  in
  let f_name = f_name name in
  { Named.name = f_name; data = type_ }
;;

let impl { Named.name; data = fields } =
  let make =
    let fields =
      List.map fields ~f:(fun (field : Ml.Type.field) ->
        let open Ml.Expr in
        field.name, Create (Ident field.name))
    in
    Ml.Expr.Create (Record fields)
  in
  let pat =
    let need_unit = need_unit fields in
    let fields =
      List.map fields ~f:(fun (field : Ml.Type.field) ->
        match field.typ with
        | Optional t -> Ml.Arg.Optional (field.name, field.name), t
        | t -> Ml.Arg.Labeled (field.name, field.name), t)
    in
    if need_unit
    then
      (* Gross hack because I was too lazy to allow patterns in toplevel
         exprs *)
      fields @ [ Unnamed "()", Ml.Type.unit ]
    else fields
  in
  let body = { Ml.Expr.pat; type_ = Ml.Type.name name; body = make } in
  let f_name = f_name name in
  { Named.name = f_name; data = body }
;;

let impl_of_type (t : Ml.Type.decl Named.t) =
  match (t.data : Ml.Type.decl) with
  | Record fields ->
    let create = impl { t with data = fields } in
    [ { create with data = Ml.Module.Value create.data } ]
  | _ -> []
;;

let intf_of_type (t : Ml.Type.decl Named.t) : Ml.Module.sig_ Named.t list =
  match (t.data : Ml.Type.decl) with
  | Record fields ->
    let create = intf { t with data = fields } in
    [ { create with data = Ml.Module.Value create.data } ]
  | _ -> []
;;
