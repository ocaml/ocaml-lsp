# Unreleased

## Features

- Code actions for jumping to related files (`.ml`, `.mli`, etc.) (#795)

# 1.12.4

- Allow cancellation of workspace symbols requests (#777)

- Fix unintentionally interleaved jsonrpc IO that would corrupt the session
  (#786)

- Ignore `SIGPIPE` . (#788)

# 1.12.3

## Fixes

- Fix a bad interaction between inferred interfaces and promotion code actions
  in watch mode (#753)

- Fix URI parsing (#739 fixes #471 and #459)

# 1.12.2

## Fixes

- Fix shutting down an already closed socket (#740)

# 1.12.1

## Fixes

- Fix preprocessing, ppx, and reason support (#735 fixes #696, #706)

- Support `include` in folding ranges (#730)

# 1.12.0

## Features

- Fix cancellation mechanism for all requests (#707)

- Allow cancellation of formatting requests (#707)

- Add `--fallback-read-dot-merlin` to the LSP Server (#705). If `ocamllsp` is
  started with this new flag, it will fall back to looking for Merlin
  configuration in `.merlin` files rather than calling `dune ocaml-merlin`.
  (#705)

- Support folding more ranges (#692)

# 1.11.6

## Fixes

- Stop leaking file descriptors like a sieve (#701)

# 1.11.5

- Fix process termination. Once the lsp server is stepped, the process will
  gracefully terminate (#697, fixes #694)

- Forward stderr from dune's merlin configuration to the lsp server's stderr
  (#697)

# 1.11.4

## Fixes

- Fix bug with large buffers being resized incorrectly in Lev

- Add folding ranges for more AST types (#680)

# 1.11.3

## Fixes

- Enable dune rpc integration by default (#691, fixes #690)

# 1.11.2

## Fixes

- Fix running external processes on Windows

# 1.11.1

## Fixes

- Fix Uri handling on Windows

- Fix build on MSVC 2015

# 1.11.0

## Features

- Add support for dune in watch mode. The lsp server will now display build
  errors in the diagnostics and offer promotion code actions.

- Re-introduce ocamlformat-rpc (#599, fixes #495)

## Fixes

- Fix workspace symbols that could have a wrong path in some cases
  ([#675](https://github.com/ocaml/ocaml-lsp/pull/671))

# 1.10.6

## Fixes

- Compatiblity with OCaml 4.14.0

# 1.10.5

## Fixes

- Patch merlin to remove the result module

# 1.10.4

## Fixes

- Use newer versions of ocamlformat-rpc-lib (fixes #697)

# 1.10.3

## Fixes

- Fix more debouncing bugs (#629)

# 1.10.2

## Fixes

- Catch merlin desturct exceptions (#626)

- Fix broken debouncing (#627)

# 1.10.1

## Fixes

- Fix executing ppx executables

# 1.10.0

## Features

- Add better support for code folding: more folds and more precise folds

## Fixes

- Fix infer interface code action crash when implementation source does not
  exist (#597)

- Improve error message when the reason plugin for merlin is absent (#608)

- Fix `chdir` races when running ppx (#550)

- More accurate completion kinds.
  New completion kinds for variants and fields. Removed inaccurate completion
  kinds for constructors and types. (#510)

- Fix handling request cancellation (#616)

# 1.9.1

## Fixes

- Disable functionality reliant on ocamlformat-rpc for now (#555)

- 4.13 compatiblity

# 1.9.0 (11/21/2021)

## Fixes

- Ppx processes are now executed correctly (#513)

## Breaking Change

- ocamllsp drops support for `.merlin` files, and as a consequence no longer
  depends on dot-merlin-reader. (#523)

## Features

- New code action to automatically remove values, types, opens (#502)

# 1.8.3 (09/26/2021)

- Fix debouncing of document updates. It was essentially completely broken in
  all but the most trivial cases. (#509 fixes #504)

- Fix completion when passing named and functional arguments (#512)

# 1.8.2 (09/14/2021)

- Disable experimental dune support. It was accidentally left enabled.

# 1.8.1 (09/12/2021)

- Update to latest merlin.

# 1.8.0 (08/19/2021)

## Fixes

- Handle workspace change notifications. Previously, the server would only use
  the set of workspaces given at startup to search for workspace symbols. After
  this change, workspace folders that are added later will also be considered.
  (#498)

## Features

- Add a new code action `Add missing rec keyword`, which is available when
  adding a `rec` keyword can fix `Unbound value ...` error, e.g.,

  ```ocaml
  let fact n = if n = 0 then 1 else n * fact (n - 1)
                                     (* ^^^^ Unbound value fact *)
  ```

  Adding `rec` to the definition of `fact` will fix the problem. The new code
  action offers adding `rec`.

- Use ocamlformat to properly format type snippets. This feature requires the
  `ocamlformat-rpc` opam package to be installed. (#386)

- Add completion support for polymorphic variants, when it is possible to pin
  down the precise type. Examples (`<|>` stands for the cursor) when completion
  will work (#473)

  Function application:

  ```
  let foo (a: [`Alpha | `Beta]) = ()

  foo `A<|>
  ```

  Type explicitly shown:

  ```
  let a : [`Alpha | `Beta] = `B<|>
  ```

  Note: this is actually a bug fix, since we were ignoring the backtick when
  constructing the prefix for completion.

- Parse merlin errors (best effort) into a more structured form. This allows
  reporting all locations as "related information" (#475)

- Add support for Merlin `Construct` command as completion suggestions, i.e.,
  show complex expressions that could complete the typed hole. (#472)

- Add a code action `Construct an expression` that is shown when the cursor is
  at the end of the typed hole, i.e., `_|`, where `|` is the cursor. The code
  action simply triggers the client (currently only VS Code is supported) to
  show completion suggestions. (#472)

- Change the formatting-on-save error notification to a warning notification
  (#472)

- Code action to qualify ("put module name in identifiers") and unqualify
  ("remove module name from identifiers") module names in identifiers (#399)

  Starting from:

  ```ocaml
  open Unix

  let times = Unix.times ()
  let f x = x.Unix.tms_stime, x.Unix.tms_utime
  ```

  Calling "remove module name from identifiers" with the cursor on the open
  statement will produce:

  ```ocaml
  open Unix

  let times = times ()
  let f x = x.tms_stime, x.tms_utime
  ```

  Calling "put module name in identifiers" will restore:

  ```ocaml
  open Unix

  let times = Unix.times ()
  let f x = x.Unix.tms_stime, x.Unix.tms_utime
  ```

## Fixes

- Do not show "random" documentation on hover

  - fixed by [merlin#1364](https://github.com/ocaml/merlin/pull/1364)
  - fixes duplicate:
    - [ocaml-lsp#344](https://github.com/ocaml/ocaml-lsp/issues/344)
    - [vscode-ocaml-platform#111](https://github.com/ocamllabs/vscode-ocaml-platform/issues/111)

- Correctly rename a variable used as a named/optional argument (#478)

- When reporting an error at the beginning of the file, use the first line not
  the second (#489)

# 1.7.0 (07/28/2021)

## Features

- Add sub-errors as "related" information in diagnostics (#457)

- Add support for navigating to a symbol inside a workspace (#398)

- Show typed holes as errors

  Merlin has a concept of "typed holes" that are syntactically represented as `_`. Files
  that incorporate typed holes are not considered valid OCaml, but Merlin and OCaml-LSP
  support them. One example when such typed holes can occur is when on "destructs" a value,
  e.g., destructing `(Some 1)` will generate code `match Some 1 with Some _ -> _ | None -> _`. While the first underscore is a valid "match-all"/wildcard pattern, the rest of
  underscores are typed holes.

# 1.6.1 (05/17/2020)

## Fixes

- Switch `verbosity` from 1 to 0. This is the same default that merlin uses.
  The old value for verbosity (#433)

- Get fresh diagnostics (warning and error messages) on a file save (#438)

  Note: If you want the fresh diagnostics to take into account changes in other
  files, you likely need to rebuild your project. An easy way to get automatic
  rebuilds is to run `dune` in a watching mode, e.g.,[dune build --watch].

# 1.6.0 (04/30/2020)

## Features

- Code action to annotate a value with its type (#397)

## Fixes

- Fix interface/implementation switching on Windows (#427)

- Correctly parse project paths with spaces and other special characters that
  must be escaped.

- Print types with `-short-paths` even if the project wasn't built yet

# 1.5.0 (03/18/2020)

- Support 4.12 and drop support for all earlier versions

- Update to the latest version of merlin

# 1.4.1 (03/16/2020)

## Fixes

- Backport fixes from merlin (#382, #383)

- Encode request & notification `params` in a list. This is required by the
  spec. (#351)

# 1.4.0 (12/17/2020)

## Features

- Support cancellation notifications when possible. (#323)

- Implement signature help request for functions (#324)

- Server LSP requests & notifications concurrently. Requests that require merlin
  are still serialized. (#330)

# 1.3.0 (11/23/2020)

## Features

- Code action to insert inferred module interface (#308)

- Filter keywords by context (#307)

# 1.2.0 (11/16/2020)

## Features

- Add keyword completion

- Add go to declaration functionality to jump to a value's specification in a
  .mli file (#294)

## Fixes

- #245: correctly use mutexes on OpenBSD (#264)

- #268: Do not use vendored libraries when building the lsp package (#260)

- #271: Clear diagnostics when files are closed

- Disable non-prefix completion. There's no reliably way to trigger it and it
  can be slow.

# 1.1.0 (10/14/2020)

## Features

- Implement a command to switch between module interfaces and implementations
  (#254)

## Fixes

- Do not crash on invalid positions (#248)

- add missing record fields to list of completions (#253)

- do not offer `destruct` as a code action in interface files (#255)

# 1.0.0 (08/28/2020)

- Initial Release
