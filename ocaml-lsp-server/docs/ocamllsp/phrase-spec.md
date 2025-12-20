# Phrase Request

## Description

This custom request returns the position of the next or previous
phrase (top-level definition or module definition).

## Client Capability

There is no client capability relative to this request.

## Server capability

- propert name: `handlePhrase`
- property type: `boolean`

## Request

- method: `ocamllsp/phrase`
- params:

  ```json
  {
    "uri": TextDocumentIdentifier,
    "position": Position,
    "target": <"next" | "prev">,
  }
  ```
## Response

- result: `Position | null`
