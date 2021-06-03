# Unreleased

## Features

- Add sub-errors as "related" information in diagnostics (#457)

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
