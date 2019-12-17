# OCaml-LSP

This project contains an implementation of an LSP server for OCaml and a
standalone implementation of the LSP protocol.

## Installation

We recommend to install the server via a project such as
[opam](http://github.com/ocaml/opam) or [esy](https://github.com/esy/esy).

### Opam

To install the lsp server in a particular opam switch:

```
$ opam install ocaml-lsp-server
```

Note that you will need to run install the lsp server in every switch where
you'd like to use it.

### Source

This project uses submodules to handle dependencies. This is done so that users
who install ocaml-lsp-server into their sandbox will not share constraints on
the same dependencies that ocaml-lsp-server is using.

```
$ git clone --recurse-submodules http://github.com/ocaml/ocaml-lsp.git
$ cd ocaml-lsp
$ dune build
```

## Features

The server supports the following queries:

- [x] `textDocument/completion`
- [ ] `completionItem/resolve`
- [x] `textdocument/hover`
- [ ] `textDocument/signatureHelp`
- [x] `textDocument/definition`
- [x] `textDocument/typeDefinition`
- [ ] `textDocument/implementation`
- [x] `textDocument/codeLens`
- [x] `textDocument/documentHighlight`
- [x] `textDocument/documentSymbol`
- [x] `textDocument/references`
- [ ] `textDocument/documentColor`
- [ ] `textDocument/colorPresentation`
- [ ] `textDocument/formatting`
- [ ] `textDocument/rangeFormatting`
- [ ] `textDocument/onTypeFormatting`
- [ ] `textDocument/prepareRename`
- [ ] `textDocument/foldingRange`

Note that degrees of support for each LSP request are varying.

## Tests

## Relationship to Other Tools

The lsp server uses merlin under the hood, but users are not required to have
merlin installed. We vendor merlin because we currently heavily depend on some
implementation details of merlin that make infeasible to upgrade the lsp server
and merlin independently.

## History

The implementation of the lsp protocol itself was taken from [facebook's
hack](https://github.com/facebook/hhvm/blob/master/hphp/hack/src/utils/lsp/lsp.mli)

Previously, this lsp server was a part of merlin, until it was realized that the
lsp protocol covers a wider scope than merlin.
