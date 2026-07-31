open Import

type ('key, 'value) t =
  { entries : ('key, 'value Lazy_fiber.t) Hashtbl.t
  ; f : 'key -> 'value Fiber.t
  }

let create key ~f = { entries = Hashtbl.create key; f }

let get t key =
  Hashtbl.find_or_add t.entries key ~default:(fun () ->
    Lazy_fiber.create (fun () -> t.f key))
  |> Lazy_fiber.force
;;
