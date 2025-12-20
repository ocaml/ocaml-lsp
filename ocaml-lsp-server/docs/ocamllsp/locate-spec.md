# Locate Request

## Description

The LSP protocol natively allows searching for definitions and
declarations; however, Merlin's `locate` command allows passing a
prefix to search for identifiers, hence the presence of this custom
request to offer more control in certain contexts.

## Client Capability

There is no client capability relative to this request.

## Server capability

- propert name: `handleLocate`
- property type: `boolean`

## Request

- method: `ocamllsp/locate`
- params:

  ```json
  {
    "uri": TextDocumentIdentifier,
    "position": Position,
    "kind": <"definition" | "declaration" | "type-definition">,
    "prefix?": string
  }
  ```
## Response

```json
[
    { "range": Range,
      "uri": DocumentIdentifier  }
]
```
