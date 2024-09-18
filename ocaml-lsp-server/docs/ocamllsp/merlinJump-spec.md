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
export interface Jump extends Location {
    uri: string;
    range: {
        start: Position;
        end: Position;
    }
}
```

- result:
    - Type: Jump or string
    - Description: If the jump is successful, a list of locations is returned where the first location is the most relevant one. If no relevant jump location is found, the result will be a string "no matching target" or an error message.
    - Jump:
        - Type: Location[], A list of Location objects representing the potential targets of the jump.
            - Location:
                - uri: The URI of the document where the jump target is located.
                - range: The range within the document where the jump target is located. Both start and end positions are the same as the jump target location.
    The Location type is the same type returned by `Goto` requests such as `goto-definition`, `goto-declaration` and `goto-typeDefinition`.
