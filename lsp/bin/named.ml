type 'a t =
  { name : string
  ; data : 'a
  }

let make ~name data = { name; data }

let data t = t.data

let name t = t.name

let map t ~f = { t with data = f t.data }

let set_data t data = { t with data }

let to_dyn f { name; data } =
  let open Stdune.Dyn.Encoder in
  record [ ("name", String name); ("data", f data) ]
