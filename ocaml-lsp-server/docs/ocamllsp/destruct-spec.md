# Construct Request

## Description

Provides a query to performs `case analysis` on pattern matching
clauses.

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

Contain the `range` to be substitute and the `content`.
