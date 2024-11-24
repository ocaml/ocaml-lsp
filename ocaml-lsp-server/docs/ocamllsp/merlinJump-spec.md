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
    - `TextDocument`: Specifies the document for which the request is sent. It includes a uri property that points to the document.
    - `Position`: Specifies the position in the document for which the documentation is requested. It includes line and character properties.
    More details can be found in the [Position - LSP Specification](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#position) and [TextDocument](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentIdentifier)

## Response

```js
result: Some Jump list | None
export interface Jump extends TextDocumentPositionParams {
}
```

- result:
    - Type: Some Jump list or None
    - Description: All succesful jumps are accumulated in a list of (`target`,`position`) and returned or `None` indicating no valid targets.
    - Jump:
        - Type:
            - `Position`: The position to jump to.
            - `target`: The target which corresponds to this jump.
