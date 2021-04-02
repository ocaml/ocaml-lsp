# Unreleased

## Features

- Code action to annotate a value with its type (#397)

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
