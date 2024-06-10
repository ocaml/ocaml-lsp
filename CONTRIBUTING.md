# Contributing to ocaml-lsp

OCaml-lsp is a community oriented open-source project and we encourage and value
any kind of contribution. Thanks for taking the time to contribute 🐫 !

## Code of Conduct

OCaml-lsp  adheres to the OCaml Code of Conduct as stated in the [Code of Conduct
document](CODE_OF_CONDUCT.md). By participating, you are expected to uphold this
code. Please report unacceptable behavior either to local contacts (listed in
[here](CODE_OF_CONDUCT.md)) or to someone listed in the upstream [OCaml Code of
Conduct](CODE_OF_CONDUCT.md).

## Documentation

Much of the information relating to the repository, such as installation
guidelines, how to set up a development environment and how to run unit tests,
can be found in the project [README](README.md). And custom requests are
documented in the
[ocaml-lsp-server/docs/ocamllsp](ocaml-lsp-server/docs/ocamllsp) directory.

Contributions to the documentation are welcome!


## Question, bug reports and feature requests

We rely on [Github's issue tracker](https://github.com/ocaml/ocaml-lsp/issues) for
support questions, feature requests and bug reports.

When reporting an issue, please include a precise reproduction in the bug report
when that's possible, as it is a very useful tool to investigate. You should
also check that you are using the latest version of OCaml-lsp and that a similar
issue has not already been submitted.

## Code contributions

### Styleguides

- **OCaml**: a large part of the code base is written in OCaml and the project
  is configured to work with
  [ocamlformat](https://ocaml.org/p/ocamlformat/latest) (version defined in the
  [`.ocamlformat` file](.ocamlformat)).

- **TypeScript**: TypeScript is used to describe certain end-to-end tests
  (abbreviated as `e2e`) and the project uses the
  [prettier](https://prettier.io/) formatter. But the TypeScript testsuite is
  deprecated (we do not allow extending them anymore. Gradually we'll rewrite
  them all to OCaml).

Apart from that, the project tries to apply implicit conventions, at the
decision of the maintainers. At the same time, it tries to follow certain naming
conventions:

- use of `t` manifest types in modules, when it makes sense;
- conversion functions respecting the naming scheme: `to_xxx` or `of_xxx`;
-  When you want to provide a conversion function for JSON, use the following
   convention: `t_of_yojson` and `yojson_of_t` to fit properly with
   `ppx_yojson_conv`.

Changes unrelated to the issue addressed by a PR should be made in a separate
PR. Additionally, formatting changes in parts of the code not concerned by a
specific PR should be proposed in another PR.

Ideally, any opened issue should be accompanied by a test with a reproduction.
When working on a fix for an issue, the first commit should contain the test
showing the issue. Following commits should fix the issue and update the test
result accordingly.

### Repository organization

The repository exposes a number of separate libraries (some of which are
internal) and vendor libraries (to reduce the dependencies required by the
project). Here is a list of the libraries exposed by the project.

#### ocaml-lsp-server

Contains the concrete implementation of a protocol server language for OCaml. A
frontend used in particular by [Visual Studio
Code](https://github.com/ocamllabs/vscode-ocaml-platform), but also by code
editors supporting LSP. The code lives mainly in the following directories:
[ocaml-lsp-server/](ocaml-lsp-server/).

In addition, the project exposes two sub-directories dedicated to [code
actions](ocaml-lsp-server/src/code_actions) and [custom
requests](https://github.com/ocaml/ocaml-lsp/tree/master/ocaml-lsp-server/src/custom_requests).

In most cases, it is likely that the contributions will focus solely on this
project.

##### Warning

For historical reasons, but also for development convenience, `ocaml-lsp-server`
should not build logic based on `Typedtree` (which changes from version to
version and migration logic is not provided by `lsp`, nor `ocaml-lsp-server` but
by [Merlin](https://github.com/ocaml/merlin)). If a command, or a constant,
relies on the `Typedtree`, it can be marked as _unstable_. Another approach,
more robust, is to build a command in Merlin that handles the logic to ensure
the migration is localized to a single project.

#### lsp

Implementation of the LSP protocol in OCaml. It is designed to be as portable as
possible and does not make any assumptions about IO. This is the implementation
of the plumbing required to describe the LSP protocol and is used by a concrete
server (for example the OCaml server) to describe the exposition of the
protocol. The code lives mainly in the following directories: [lsp/](lsp/) and
[lsp-fiber/](lsp-fiber/).

##### Warning

The set of types forming the LSP protocol API is generated automatically by a
[preprocessor](lsp/bin) based on the [protocol
specification](https://microsoft.github.io/language-server-protocol/overviews/lsp/overview/).
The pair of [types.ml](lsp/src/types.ml) and [types.mli](lsp/src/types.mli)
files must be consciously modified manually (never modifying the parts marked as
being generated by CINAPS, the preprocessor).


#### jsonrpc

Describes an implementation of the [JSON-RPC
2.0](https://www.jsonrpc.org/specification) protocol, which is mainly used as a
communication protocol for LSP. The code lives mainly in the following
directories: [jsonrpc](jsonrpc/) and [jsonrpc-fiber/](jsonrpc-fiber/).
