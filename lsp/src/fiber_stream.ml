open Import

module In = struct
  type 'a t = unit -> 'a option Fiber.t

  let create f = f

  let read t = t ()

  let empty () () = Fiber.return None

  let of_list xs =
    let xs = ref xs in
    fun () ->
      match !xs with
      | [] -> Fiber.return None
      | x :: xs' ->
        xs := xs';
        Fiber.return (Some x)

  let map t ~f () =
    let open Fiber.O in
    let+ next = read t in
    match next with
    | None -> None
    | Some x -> Some (f x)
end

module Out = struct
  type 'a t = 'a option -> unit Fiber.t

  let create f = f

  let write f x = f x

  let of_ref ref = function
    | None -> Fiber.return ()
    | Some x -> Fiber.return (ref := x :: !ref)

  let null () _ = Fiber.return ()
end

let connect i o =
  let open Fiber.O in
  let rec go () =
    let* a = In.read i in
    let* () = Out.write o a in
    go ()
  in
  go ()

let pipe () =
  let mvar = Fiber_mvar.create () in
  let i = In.create (fun () -> Fiber_mvar.get mvar) in
  let o = Out.create (fun x -> Fiber_mvar.set mvar x) in
  (i, o)
