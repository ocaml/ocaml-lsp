# Unreleased

## Features

- Jump to the first hole on calling `Destruct` code action (only with client VSCode OCaml
  Platform) (#468)

  Example: when a user invokes `Destruct` code action on `Some 1`, this code is replaced
  by `match Some 1 with None -> _ | Some _ -> _`, where the 1st and 3rd underscores
  are "typed holes", a concept created by Merlin to be able to put "holes" in OCaml code.

  With this change, now for VSCode OCaml Platform users, on such invocation of `Destruct`,
  the cursor will jump to the first typed hole and select it, so that user can start
  editing right away.

- Use ocamlformat to properly format type snippets. This feature requires the
  `ocamlformat-rpc` opam package to be installed. (#386)

- Add completion support for polymorphic variants, when it is possible to pin down the
  precise type. Examples (`<|>` stands for the cursor) when completion will work (#473)

  Function application:

  ```
  let foo (a: [`Alpha | `Beta]) = ()

  foo `A<|>
  ```

  Type explicitly shown:

  ```
  let a : [`Alpha | `Beta] = `B<|>
  ```

  Note: this is actually a bug fix, since we were ignoring the backtick when constructing
  the prefix for completion.

- Parse merlin errors (best effort) into a more structured form. This allows
  reporting all locations as "related information" (#475)

- Add support for Merlin `Construct` command as completion suggestions, i.e., show complex
  expressions that could complete the typed hole. (#472)

- Add a code action `Construct an expression` that is shown when the cursor is at the end
  of the typed hole, i.e., `_|`, where `|` is the cursor. The code action simply triggers
  the client (currently only VS Code is supported) to show completion suggestions. (#472)

# 1.7.0 (07/28/2021)

## Features

- Add sub-errors as "related" information in diagnostics (#457)

- Add support for navigating to a symbol inside a workspace (#398)

- Show typed holes as errors

  Merlin has a concept of "typed holes" that are syntactically represented as `_`. Files
  that incorporate typed holes are not considered valid OCaml, but Merlin and OCaml-LSP
  support them. One example when such typed holes can occur is when on "destructs" a value,
  e.g., destructing `(Some 1)` will generate code `match Some 1 with Some _ -> _ | None ->
  _`. While the first underscore is a valid "match-all"/wildcard pattern, the rest of
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
