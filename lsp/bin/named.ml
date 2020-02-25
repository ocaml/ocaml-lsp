type 'a t =
  { name : string
  ; data : 'a
  }

let make ~name data = { name; data }
