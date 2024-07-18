# OCaml-LSP <!-- omit from toc -->
<!-- TOC is updated automatically by "Markdown All in One" vscode extension -->

[![Build][build-badge]][build]
[![Coverage Status][coverall-badge]][coverall]

[build-badge]: https://github.com/ocaml/ocaml-lsp/actions/workflows/build-and-test.yml/badge.svg
[build]: https://github.com/ocaml/ocaml-lsp/actions
[coverall-badge]: https://coveralls.io/repos/github/ocaml/ocaml-lsp/badge.svg?branch=master
[coverall]: https://coveralls.io/github/ocaml/ocaml-lsp?branch=master

OCaml-LSP is a language server for OCaml that implements [Language Server
Protocol](https://microsoft.github.io/language-server-protocol/) (LSP).

> If you use Visual Studio Code, see OCaml Platform extension
> [page](https://github.com/ocamllabs/vscode-ocaml-platform) for detailed
> instructions on setting up your editor for OCaml development with OCaml-LSP:
> what packages need to be installed, how to configure your project and get
> most out of the OCaml editor support, and how to report and debug problems.

- [Installation](#installation)
  - [Installing with package managers](#installing-with-package-managers)
    - [Opam](#opam)
    - [Esy](#esy)
  - [Installing from sources](#installing-from-sources)
  - [Additional package installations](#additional-package-installations)
- [Usage](#usage)
  - [Integration with Dune RPC](#integration-with-dune-rpc)
  - [Merlin configuration (advanced)](#merlin-configuration-advanced)
- [Features](#features)
  - [Semantic highlighting](#semantic-highlighting)
  - [LSP Extensions](#lsp-extensions)
  - [Unusual features](#unusual-features)
- [Debugging](#debugging)
- [Contributing to project](#contributing-to-project)
  - [Changelog](#changelog)
- [Tests](#tests)
- [Relationship to Other Tools](#relationship-to-other-tools)
- [History](#history)
- [Comparison to other LSP Servers for OCaml](#comparison-to-other-lsp-servers-for-ocaml)

## Installation

Below we show how to install OCaml-LSP using opam, esy, and from sources. OCaml-LSP comes in a package called `ocaml-lsp-server` but the installed program (i.e., binary) is called `ocamllsp`.

### Installing with package managers

#### Opam

To install the language server in the currently used opam [switch](https://opam.ocaml.org/doc/Manual.html#Switches):

```sh
$ opam install ocaml-lsp-server
```

_Note:_ you will need to install `ocaml-lsp-server` in every switch where you
would like to use it.

#### Esy

To add the language server to an esy project, run in terminal:

```sh
$ esy add @opam/ocaml-lsp-server
```

### Installing from sources

This project uses submodules to handle dependencies. This is done so that users
who install `ocaml-lsp-server` into their sandbox will not share dependency
constraints on the same packages that `ocaml-lsp-server` is using.

```sh
$ git clone --recurse-submodules http://github.com/ocaml/ocaml-lsp.git
$ cd ocaml-lsp
$ make install
```

### Additional package installations

- Install [ocamlformat](https://github.com/ocaml-ppx/ocamlformat#installation)
  package if you want source file formatting support.

  Note: To have source file formatting support in your project, there needs to
  be an `.ocamlformat` file present in your project's root directory.

- OCaml-LSP also uses a program called `ocamlformat-rpc` to format code that is
  either generated or displayed by OCaml-LSP, e.g., when you hover over a module
  identifier, you can see its typed nicely formatted. This program comes with
  `ocamlformat` (version > 0.21.0). Previously, it was a standalone package.

## Usage

Usually, your code editor, or some extension/plugin that you install on it, is
responsible for launching `ocamllsp`.

Important: OCaml Language Server has its information about the files from the
last time your built your project. We recommend using the Dune build system and
running it in "watch" mode to always have correctly functioning OCaml-LSP, e.g.,
`dune build --watch`.

### Integration with Dune RPC

> since OCaml-LSP 1.11.0

OCaml-LSP can communicate with Dune's RPC system to offer some interesting
features. User can launch Dune's RPC system by running Dune in watch mode.
OCaml-LSP will *not* launch Dune's RPC for you. But OCaml-LSP will see if there
is an RPC running and will communicate with it automatically.

There are various interesting features and caveats:

1. Dune's RPC enables new kinds of diagnostics (i.e., warnings and errors) to be
   shown in the editor, e.g., mismatching interface and implementation files.
   You need to save the file to refresh such diagnostics because Dune doesn't
   see unsaved files; otherwise, you may see stale (no longer correct) warnings
   or errors. OCaml-LSP updates diagnostics after each build is complete in
   watch mode.

2. Dune file promotion support. If you, for example, use `ppx_expect` and have
   failing tests, you will get a diagnostic when Dune reports that your file can
   be promoted. You can promote your file using the code action `Promote`.

### Merlin configuration (advanced)

If you would like OCaml-LSP to respect your `.merlin` files, OCaml-LSP needs to
be invoked with `--fallback-read-dot-merlin` argument passed to it and you must
have the `dot-merlin-reader` package installed.

## Features

<!-- TODO:
  this is quite a large list (which becomes even larger since it's missing some requests), which is not necessarily of big interest to users.

  We should consider:
  1. Moving it to the bottom
  2. Converting it into a table

     | Description      | Method                  | OCaml | Reason | Dune | Menhir | .ocamlformat | ...
     | Auto-completion  | textDocument/completion |   x   |    x   |   o  |   o    |      o       | ...

  3. (not sure how) Generate the table automatically because, otherwise, it's outdated frequently.
-->

The server supports the following LSP requests (inexhaustive list):

- [x] `textDocument/completion`
- [x] `completionItem/resolve`
- [x] `textdocument/hover`
- [ ] `textDocument/signatureHelp`
- [x] `textDocument/declaration`
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
- [x] `workspace/didChangeConfiguration`
- [x] `workspace/symbol`

Note that degrees of support for each LSP request are varying.

## Configuration

[Read more about configurations supported by ocamllsp](./ocaml-lsp-server/docs/ocamllsp/config.md)

### Semantic highlighting

> since OCaml-LSP 1.15.0 (since version `1.15.0-4.14` for OCaml 4, `1.15.0-5.0` for OCaml 5)

Semantic highlighting support is enabled by default.

> since OCaml-LSP 1.14.0

OCaml-LSP implements experimental semantic highlighting support (also known as
semantic tokens support). The support can be activated by passing an evironment
variable to OCaml-LSP:

- To enable non-incremental (expectedly slower but more stable) version, pass
  `OCAMLLSP_SEMANTIC_HIGHLIGHTING=full` environment variable to OCaml-LSP.

- To enable incremental (potentially faster but more error-prone, at least on VS
  Code) version, pass `OCAMLLSP_SEMANTIC_HIGHLIGHTING=full/delta` to OCaml-LSP.

Tip (for VS Code OCaml Platform users): You can use `ocaml.server.extraEnv`
setting in VS Code to pass various environment variables to OCaml-LSP.

```json
{
    "ocaml.server.extraEnv": {
        "OCAMLLSP_SEMANTIC_HIGHLIGHTING": "full"
    },
}
```

### LSP Extensions

The server also supports a number of OCaml specific extensions to the protocol:
- [Switch to implementation/interface](ocaml-lsp-server/docs/ocamllsp/switchImplIntf-spec.md)
- [Infer interface](ocaml-lsp-server/docs/ocamllsp/inferIntf-spec.md)
- [Locate typed holes](ocaml-lsp-server/docs/ocamllsp/typedHoles-spec.md)
- [Find wrapping AST node](ocaml-lsp-server/docs/ocamllsp/wrappingAstNode-spec.md)

Note that editor support for these extensions varies. In general, the OCaml Platform extension for Visual Studio Code will have the best support.

### Unusual features

#### Destructing a value <!-- omit in toc -->

> since OCaml-LSP 1.0.0

OCaml-LSP has a code action that allows to generate an exhaustive pattern
matching for values. For example, placing a cursor near a value `(Some 10)|`
where `|` is your cursor, OCaml-LSP will offer a code action "Destruct", which
replaces `(Some 10)` with `(match Some with | None -> _ | Some _ -> _)`.
Importantly, one can only destruct a value if OCaml-LSP can infer the value's
precise type. The value can be type-annotated, e.g., if it's a function argument
with polymorphic (or yet unknown) type in this context. In the code snippet
below, we type-annotate the function parameter `v` because when we type `let f v
= v|`, the type of `v` is polymorphic, so we can't destruct it.

You can also usually destruct the value by placing the cursor on the wildcard
(`_`) pattern in a pattern-match. For example,

```ocaml
type t = A | B of string option

let f (v : t) = match v with | A -> _ | B _| -> _
```

invoking destruct near the cursor (`|`) in the snippet above, you get

```ocaml
type t = A | B of string option

let f (v : t) = match v with | A -> _ | B (None) | B (Some _) -> _
```

Importantly, note the undescores in place of expressions in each branch of the
pattern match above. The underscores that occur in place of expressions are
called "typed holes" - a concept explained below.

Tip (formatting): generated code may not be greatly formatted. If your project
uses a formatter such as OCamlFormat, you can run formatting and get a
well-formatted document (OCamlFormat supports typed holes formatting).

Tip (for VS Code OCaml Platform users): You can destruct a value using a keybinding
<kbd>Alt</kbd>+<kbd>D</kbd> or on MacOS <kbd>Option</kbd>+<kbd>D</kbd>

#### Typed holes <!-- omit in toc -->

> since OCaml-LSP 1.8.0

OCaml-LSP has a concept of a "typed hole" syntactically represented as `_`
(underscore). A typed hole represents a well-typed "substitute" for an
expression. OCaml-LSP considers these underscores that occur in place of
expressions as a valid well-typed OCaml program: `let foo : int = _` (the typed
hole has type `int` here) or `let bar = _ 10` (the hole has type `int -> 'a`).
One can use such holes during development as temporary substitutes for
expressions and "plug" the holes later with appropriate expressions.

Note, files that incorporate typed holes are *not* considered valid OCaml by the
OCaml compiler and, hence, cannot be compiled.

Also, an underscore occurring in place of a pattern (for example `let _ = 10`)
should not be confused with a typed hole that occurs in place of an expression,
e.g., `let a = _`.

#### Constructing values by type (experimental) <!-- omit in toc -->

> since OCaml-LSP 1.8.0

OCaml-LSP can "construct" expressions based on the type required and offer them
during auto-completion. For example, typing `_` (typed hole) in the snippet
below will trigger auto-completion (`|` is your cursor):

```ocaml
(* file foo.ml *)
type t = A | B of string option

(* file bar.ml *)
let v : Foo.t = _|
```

The auto-completion offers completions `Foo.A` and `Foo.B _`. You can further
construct values by placing the cursor as such: `Foo.B _|` and triggering code
action "Construct an expression" which offers completions `None` and `Some _`.
Trigger the same code action in `Some _|` will offer `""` - one of the possible
expressions to replace the typed hole with.

Constructing a value is thus triggered either by typing `_` in place of an
expression or trigger the code action "Construct an Expression". Also, the type
of the value needs to be non-polymorphic to construct a meaningful value.

Tip (for VS Code OCaml Platform users): You can construct a value using a keybinding
<kbd>Alt</kbd>+<kbd>C</kbd> or on MacOS <kbd>Option</kbd>+<kbd>C</kbd>

#### Syntax Documentation

> since OCaml-LSP 1.18.0

OCaml-LSP can display documentation about the node under the cursor when
the user hovers over some OCaml code. For example, hovering over the code
snippet below will display some information about what the syntax
is:

```ocaml
type point = {x: int; y: int}
```
Hovering over the above will
display:
```
ocaml type point = { x : int; y : int } 
syntax Record type:
Allows you to define variants with a fixed set of fields, and all of the
constructors for a record variant type must have the same fields. See
Manual
```
The documentation is gotten from the Merlin engine which receives
the nodes under the cursor and infers what the syntax may be about, and
displays the required information along with links to the manual for further
reading. 

Syntax Documentation is an optional feature and can be activated by
using the LSP config system with the key called `syntaxDocumentation` and can
be enabled via setting it to `{ enable: true }`.

## Debugging

If you use Visual Studio Code, please see OCaml Platform extension
[page](https://github.com/ocamllabs/vscode-ocaml-platform) for a detailed guide
on how to report and debug problems.

If you use another code editor and use OCaml-LSP, you should be able to set the
server trace to `verbose` using your editor's LSP client and watch the trace
for errors such as logged exceptions.

## Contributing to project

```bash
# clone repo with submodules
git clone --recursive git@github.com:ocaml/ocaml-lsp.git

cd ocaml-lsp

# if you already cloned, pull submodules
git submodule update --init --recursive

# create local switch (or use global one)
opam switch --yes create . ocaml-base-compiler.4.14.0

# don't forget to set your environment to use the local switch
eval $(opam env)

# install dependencies
make install-test-deps

# build
make all

# the ocamllsp executable can be found at _build/default/ocaml-lsp-server/bin/main.exe
```

### Changelog

User-visible changes should come with an entry in the changelog under the appropriate part of
the **unreleased** section. PR that doesn't provide an entry will fail CI check. This behavior
can be overridden by using the "no changelog" label, which is used for changes that are not user-visible.

## Tests

To run tests execute:

```sh
$ make test
```

Note that tests require [Node.js](https://nodejs.org/en/) and
[Yarn](https://yarnpkg.com/lang/en/) installed.

## Relationship to Other Tools

The lsp server uses merlin under the hood, but users are not required to have
merlin installed. We vendor merlin because we currently heavily depend on some
implementation details of merlin that make it infeasible to upgrade the lsp
server and merlin independently.

## History

The implementation of the lsp protocol itself was taken from
[facebook's hack](https://github.com/facebook/hhvm/blob/master/hphp/hack/src/utils/lsp/lsp.mli)

Previously, this lsp server was a part of merlin, until it was realized that
the lsp protocol covers a wider scope than merlin.

## Comparison to other LSP Servers for OCaml

Note that the comparisons below make no claims of being objective and may be
entirely out of
date. Also, both servers seem deprecated.

- [reason-language-server](https://github.com/jaredly/reason-language-server)
  This server supports
  [bucklescript](https://github.com/BuckleScript/bucklescript) &
  [reason](https://github.com/facebook/reason). However, this project does not
  use merlin which means that it supports fewer versions of OCaml and offers less
  "smart" functionality - especially in the face of sources that do not yet
  compile.

- [ocaml-language-server](https://github.com/ocaml-lsp/ocaml-language-server)
  This project is extremely similar in the functionality it provides because it
  also reuses merlin on the backend. The essential difference is that this
  project is written in typescript, while our server is in OCaml. We feel that
  it's best to use OCaml to maximize the contributor pool.
