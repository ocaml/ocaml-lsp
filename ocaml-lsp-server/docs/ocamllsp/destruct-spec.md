# Construct Request

## Description

Provides a query that performs `case analysis` to generate or 
complete pattern matchings. See [Merlin's documentation](https://github.com/ocaml/merlin/blob/main/doc/dev/PROTOCOL.md#case-analysis--start-position--end-position).

## Client Capability

There is no client capability relative to this request.

## Server capability

- property name: `handleDestruct`
- property type: `boolean`

## Request

- method: `ocamllsp/destruct`
- params:

  ```json
  {
    "uri": TextDocumentIdentifier,
    "range": Range
  }
  ```

## Response

```json
{
  "range": Range,
  "content": string
}
```

Contains the `range` to be substituted and the new `content`.
