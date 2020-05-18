open Import

module In = struct
  type 'a t = unit -> 'a option Fiber.t

  let create f = f

  let read t = t ()

  let of_list xs =
    let xs = ref xs in
    fun () ->
      match !xs with
      | [] -> Fiber.return None
      | x :: xs' ->
        xs := xs';
        Fiber.return (Some x)

  let return x =
    let first = ref true in
    fun () ->
      if !first then (
        first := false;
        Fiber.return (Some x)
      ) else
        Fiber.return None
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
