# Type Expression Request

## Description

Returns the type of a given expression as a string.

## Client capability

There is no client capability relative to this request.

## Server capability

- property name: `handleTypeExpression`
- property type: `boolean`

## Request

- method: `ocamllsp/typeExpression`
- params:

  ```json
  {
    "uri": TextDocumentIdentifier,
    "position": Position,
    "expression": string,
  }
  ```

## Response

- result: `string | null`
