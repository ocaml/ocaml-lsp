# Merlin Call Compatible Request

## Description

Allows Merlin commands to be invoked from LSP, in the same way as the
`ocamlmerlin` binary, using a custom request. Invoking this command returns the
result in the form of a character string (which can be JSON or SEXP)
representing the result of a Merlin command. This makes it possible to implement
clients capable of fallbacking on Merlin in the event of a missing feature.

### Why this custom request needed

It allows editor plugin to communicate with the ocaml-lsp-server using the
merlin protocol, it will be useful for text-based editors that want to preserve
the classic Merlin UI while using ocaml-lsp-server. (It is a temporary solution
that will progressively be replaced by tailored custom requests filling the gaps
in the protocol)

## Client capability

There is no client capability relative to this request

## Server capability

property name: `handleMerlinCallCompatible`

property type: `boolean`

## Request

- method: `ocamllsp/merlinCallCompatible`
- params:

```json
{
  "uri": DocumentUri,
  "command": string,
  "args": string[],
  "resultAsSexp": boolean
}
```

- `uri`: is the reference of the current document
- `command`: is the name of the command invoked (ie: `case-analysis`)
- `args`: all the parameters passed to the command, by default: `[]`
- `resultAsSexp`: a flag indicating whether the result should be returned in
  SEXP (`true`) or JSON (`false`), by default: `false`

For an exhaustive description of what the query returns, please refer to the
[Merlin
protocol](https://github.com/ocaml/merlin/blob/master/doc/dev/PROTOCOL.md)

## Response

```json
{
  "resultAsSexp": boolean,
  "result": string
}
```

- `resultAsSexp`: `true` if the command was invoked with the `resultAsSexp` flag,
  `false` otherwise
- `result`: the result in string (in JSON or SEXP)
