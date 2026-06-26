# Merlin Jump Request

## Description

This custom request allows Merlin-type code navigation in a source buffer. It will fetch all the possible jump targets and return them.

## Server capability

- propert name: `handleJump`
- property type: `boolean`

## Request

- method: `ocamllsp/jump`
- params: [TextDocumentPositionParams](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentPositionParams)

## Response

- result: `Jump`

```js

export interface TargetPosition {
    /**
     * The target's kind.
     */
    target: string;

    /**
     * The corresponding position in the request's document.
     */
    position: Position;
}

export interface Jump {
    /**
     * The list of possible targets to jump-to.
     */
    jumps: TargetPosition[]
}
```
