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
#include "lev/examples/readme.ml"
```

## Documentation

Lev's API is a thin wrapper around libev itself. So you should first and
foremost refer to libev's extensive [documentation](libevdoc). Lev itself will
document where it differs from libev's conventions.

## License

`vendor/` is under Marc Lehmann's original terms (see vendor/LICENSE).

Everything else is offered under ISC (see src/LICENSE.md).
