# Lev - OCaml bindings to libev

[libev]: http://software.schmorp.de/pkg/libev.html
[libevdoc]: http://pod.tst.eu/http://cvs.schmorp.de/libev/ev.pod

## Abstract

[libev](libev) by Marc Lehmann is a minimal & portable event loop library. This
repository contains two packages. The first is `lev` which offers low level
bindings to this library. The bindings are designed to be minimal, low
overhead, and easily embeddable in larger projects. The API is callback based
so you need to BYOC (bring your own concurrency).

The second package is `lev-fiber`. It provides a more familiar, higher level
API. It is based on dune's fiber library for structured concurrency.

## Example

This example of the low level API lev api:

```ocaml
open Lev

let () =
  let loop = Loop.default () in
  let stdin, stdin_w = Unix.pipe ~cloexec:true () in
  let stdout_r, stdout = Unix.pipe ~cloexec:true () in
  let stderr_r, stderr = Unix.pipe ~cloexec:true () in
  Unix.close stdin_w;
  Unix.close stdout_r;
  Unix.close stderr_r;
  let pid =
    Unix.create_process "sh" [| "sh"; "-c"; "exit 42" |] stdin stdout stderr
  in
  let child =
    match Child.create with
    | Error `Unimplemented -> assert false
    | Ok create ->
        create
          (fun t ~pid status ->
            Child.stop t loop;
            match status with
            | Unix.WEXITED i -> Printf.printf "%d exited with status %d\n" pid i
            | _ -> assert false)
          (Pid pid) Terminate
  in
  Child.start child loop;
  Loop.run_until_done loop;
  Child.destroy child
```

## Documentation

Lev's API is a thin wrapper around libev itself. So you should first and
foremost refer to libev's extensive [documentation](libevdoc). Lev itself will
document where it differs from libev's conventions.

## License

`vendor/` is under Marc Lehmann's original terms (see vendor/LICENSE).

Everything else is offered under ISC (see src/LICENSE.md).
