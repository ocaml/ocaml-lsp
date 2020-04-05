# OCaml-LSP

This project contains an implementation of an LSP server for OCaml and a
standalone implementation of the LSP protocol.

## Installation

We recommend to install the server via a project such as
[opam](http://github.com/ocaml/opam) or [esy](https://github.com/esy/esy).

### Opam

To install the lsp server in a particular opam switch:

```
$ opam pin add ocaml-lsp-server https://github.com/ocaml/ocaml-lsp.git
$ opam install ocaml-lsp-server
```

Note that you will need to run install the lsp server in every switch where
you'd like to use it.

### Esy

To add the lsp server to an esy project, add the following lines to your
project's `package.json`:

```
  "devDependencies": {
    "@opam/ocaml-lsp-server": "ocaml/ocaml-lsp:ocaml-lsp-server.opam"
  }
```

Note that you need to specify a specific git commit sha from this repository
instead of `<git-commit-sha>` placeholder above.

### Source

This project uses submodules to handle dependencies. This is done so that users
who install ocaml-lsp-server into their sandbox will not share constraints on
the same dependencies that ocaml-lsp-server is using.

```
$ git clone --recurse-submodules http://github.com/ocaml/ocaml-lsp.git
$ cd ocaml-lsp
$ make build
```

## Features

The server supports the following queries:

- [x] `textDocument/completion`
- [ ] `completionItem/resolve`
- [x] `textdocument/hover`
- [ ] `textDocument/signatureHelp`
- [ ] `textDocument/declaration`
- [x] `textDocument/definition`
- [x] `textDocument/typeDefinition`
- [ ] `textDocument/implementation`
- [x] `textDocument/codeLens`
- [x] `textDocument/documentHighlight`
- [x] `textDocument/documentSymbol`
- [x] `textDocument/references`
- [ ] `textDocument/documentColor`
- [ ] `textDocument/colorPresentation`
- [x] `textDocument/formatting`
- [ ] `textDocument/rangeFormatting`
- [ ] `textDocument/onTypeFormatting`
- [x] `textDocument/prepareRename`
- [x] `textDocument/foldingRange`
- [x] `textDocument/selectionRange`
- [ ] `workspace/symbol`

Note that degrees of support for each LSP request are varying.

## Tests

To run tests execute:

```
$ make test
```

Note that tests require [Node.js][] and [Yarn][] installed.

[Node.js]: https://nodejs.org/en/
[Yarn]: https://yarnpkg.com/lang/en/

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

## Comparison to other LSP Servers for OCaml

Note that the comparisons below make no claims of being objective and may be
entirely out of date:

* [reason-language-server](https://github.com/jaredly/reason-language-server)
  This server supports
  [bucklescript](https://github.com/BuckleScript/bucklescript) &
  [reason](https://github.com/facebook/reason). However, this project does not
  use merlin which means that it supports less versions of OCaml and offers less
  "smart" functionality - especially in the face of sources that do not yet
  compile.

* [ocaml-language-server](https://github.com/ocaml-lsp/ocaml-language-server)
  This project is extremely similar in the functionality it provides because it
  also reuses merlin on the backend. The essential difference is that this
  project is written in typescript, while our server is in OCaml. We feel that
  it's best to use OCaml to maximize the contributor pool.
