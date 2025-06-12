(* TODO: Add support for ocamlformat. *)

type t = unit

let create () = ()
let stop () = Fiber.return ()
let format_type () ~typ:_ = Fiber.return (Error `No_process)
let format_doc () _doc = Fiber.return (Error `No_process)
let run ~logger:_ () = Fiber.return (Error `Disabled)
