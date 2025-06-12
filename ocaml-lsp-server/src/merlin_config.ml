(* {{{ COPYING *(

   This file is part of Merlin, an helper for ocaml editors

   Copyright (C) 2013 - 2015 Frédéric Bour <frederic.bour(_)lakaban.net> Thomas
   Refis <refis.thomas(_)gmail.com> Simon Castellan <simon.castellan(_)iuwt.fr>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   The Software is provided "as is", without warranty of any kind, express or
   implied, including but not limited to the warranties of merchantability,
   fitness for a particular purpose and noninfringement. In no event shall the
   authors or copyright holders be liable for any claim, damages or other
   liability, whether in an action of contract, tort or otherwise, arising from,
   out of or in connection with the software or the use or other dealings in the
   Software.

   )* }}} *)

open Import
open Fiber.O
module Std = Merlin_utils.Std
module Misc = Merlin_utils.Misc

let dot_merlin = ref None

type t =
  { path : string
  ; initial : Mconfig.t
  }

let destroy (_ : t) = Fiber.return ()

let create () path =
  let initial =
    let initial = Mconfig.initial in
    { initial with
      ocaml = { initial.ocaml with real_paths = false }
    ; query = { initial.query with verbosity = Smart }
    }
  in
  match !dot_merlin with
  | Some path -> { path; initial }
  | None ->
    let path =
      let path = Uri.to_path path in
      Misc.canonicalize_filename path
    in
    let initial =
      let filename = Filename.basename path in
      let directory = Filename.dirname path in
      { initial with query = { initial.query with filename; directory } }
    in
    { path; initial }
;;

let config (t : t) : Mconfig.t Fiber.t =
  let+ () = Fiber.return () in
  Mconfig.get_external_config t.path t.initial
;;

module DB = struct
  type t = unit

  let get t uri = create t uri
  let create () = ()
  let run () = Fiber.return ()
  let stop () = Fiber.return ()
end
