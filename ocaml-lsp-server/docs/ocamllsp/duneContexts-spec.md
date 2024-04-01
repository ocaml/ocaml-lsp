# Dune Contexts Request

Dune Contexts Request is sent from the client to the server to get the list
of contexts available in the current Dune workspace.

Warning: this custom request is meant to be consumed by `ocaml-vscode-platform` exclusively,
it can be removed any time and should not be relied on.

## Client capability

nothing that should be noted

## Server capability

property name: `handleDuneContexts`
property type: `boolean`

## Request

- method: `ocamllsp/getDuneContexts`
- params: none

## Response

- result: String[]
- error: code and message set in case an exception happens during the processing of the request.
