# Merlin Jump Request

## Description

This custom request allows Merlin-type code navigation in a source buffer.

## Server capability

- propert name: `handleJump`
- property type: `boolean`

## Request

- method: `ocamllsp/jump`
- params: `JumpParams`  extends [TextDocumentPositionParams](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentPositionParams) and is defined as follows:

```js
export interface JumpParams extends TextDocumentPositionParams
{
    /**
     * The requested target of the jump, one of `fun`, `let`, `module`,
     * `module-type`, `match`, `match-next-case`, `match-prev-case`.
     *
     * If omitted, all valid targets will be considered.
     */
    target?: string;
}
```

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
