# OCaml-LSP

[![Build](https://github.com/ocaml/ocaml-lsp/workflows/Build%20and%20Test/badge.svg)](https://github.com/ocaml/ocaml-lsp/actions)

OCaml-LSP is a language server for OCaml that implements [Language Server
Protocol](https://microsoft.github.io/language-server-protocol/) (LSP).

> If you use Visual Studio Code, see OCaml Platform extension
> [page](https://github.com/ocamllabs/vscode-ocaml-platform) for detailed
> instructions on setting up your editor for OCaml development with OCaml-LSP:
> what packages need to be installed, how to configure your project and get
> most out of the OCaml editor support, and how to report and debug problems.

## Installation

We recommend to install the language server via a package manager such as
[opam](http://github.com/ocaml/opam) or [esy](https://github.com/esy/esy).

### Opam

To install the language server in the currently used opam [switch](https://opam.ocaml.org/doc/Manual.html#Switches):

```sh
$ opam install ocaml-lsp-server
```

_Note:_ you will need to install `ocaml-lsp-server` in every switch where you would like
to use it.

### Esy

To add the language server to an esy project, run in terminal:

```sh
$ esy add @opam/ocaml-lsp-server
```

### Source

This project uses submodules to handle dependencies. This is done so that users
who install `ocaml-lsp-server` into their sandbox will not share dependency
constraints on the same packages that `ocaml-lsp-server` is using.

```sh
$ git clone --recurse-submodules http://github.com/ocaml/ocaml-lsp.git
$ cd ocaml-lsp
$ make install
```

## Usage

Once `ocaml-lsp-server` is installed, the executable is called `ocamllsp`. For
now, the server can only be used through the standard input (`stdin`) and
output (`stdout`) file descriptors.

For an example of usage of the server in a VS Code extension, see OCaml
Platform Extension implementation
[here](https://github.com/ocamllabs/vscode-ocaml-platform/blob/master/src/vscode_ocaml_platform.ml).

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

The server supports the following LSP requests:

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
- [x] `workspace/symbol`

Note that degrees of support for each LSP request are varying.

### Semantic highlighting 

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

### Unorthodox features 

#### Destructing a value

OCaml-LSP has a code action that allows to generate an exhaustive pattern
matching for values. For example, placing a cursor near a value `(Some 10)|`
where `|` is your cursor, OCaml-LSP will offer a code action "Destruct", which
replaces `(Some 10)` with `(match Some with | None -> _ | Some _ -> _)`. 

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

#### Typed holes

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

#### Constructing values by type (experimental)

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
expression or trigger the code action "Construct an Expression".

Tip (for VS Code OCaml Platform users): You can construct a value using a keybinding
<kbd>Alt</kbd>+<kbd>C</kbd> or on MacOS <kbd>Option</kbd>+<kbd>C</kbd>

## Integration with other tools

### Source file formatting: OCamlFormat & Refmt

OCaml-LSP is dependent on external tools (OCamlFormat for OCaml and `refmt` for
Reason) for formatting source files. You should have the necessary tool
(OCamlFormat and/or Refmt) installed in your opam switch or esy project to have
formatting support. Note, however, that OCaml-LSP requires presence of
OCamlFormat configuration file, called `.ocamlformat`, in the project root to
be able to format source files in your project.

### Formatting code on hover <!-- TODO: specify until which olsp version this applies (since ofmt-rpc is inside ofmt now) -->

When you hover the cursor over OCaml code, the extension shows you the type of
the symbol. To get nicely formatted types, install
[ocamlformat-rpc](https://opam.ocaml.org/packages/ocamlformat-rpc/) package.

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
