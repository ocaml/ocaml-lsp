# Merlin Jump Request

## Description

This custom request allows Merlin-type code navigation in a source buffer.

## Server capability

- propert name: `handleMerlinJump`
- property type: `boolean`

## Request

```js
export interface JumpParams extends TextDocumentPositionParams
{
    target: string;
}
```

- method: `ocamllsp/merlinJump`
- params:
    - `TextDocumentIdentifier`: Specifies the document for which the request is sent. It includes a uri property that points to the document.
    - `Position`: Specifies the position in the document for which the documentation is requested. It includes line and character properties.
    More details can be found in the [TextDocumentPositionParams - LSP Specification](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentPositionParams).
    - `Target`: A string representing the identifier within the document to search for and jump to.

## Response

```js
result: Jump | String
export interface Jump extends TextDocumentPositionParams {
}
```

- result:
    - Type: Jump or string
    - Description: If the jump is successful, a position and document path is returned. If no relevant jump location is found, the result will be a string "no matching target" or an error message.
    - Jump:
        - Type: TextDocumentPositionParams
            - `Position`: The position to jump to
            - `TextDocumentIdentifier`: the document path which contains this position (ideally the same document as the request)
