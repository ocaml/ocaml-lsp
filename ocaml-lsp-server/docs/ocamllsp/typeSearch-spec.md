# TypeSearch Request

## Description

This custom request allows clients to perform a type search at a specific position within a text document based on finding functions or types that match a specific query pattern.

## Server capability

- property name: `handleTypeSearch`
- property type: `boolean`

## Request

```js
export interface TypeSearchParams extends TexDocumentPositionParams
{
    query: string;
    limit: int;
    with_doc: bool;
}
```
- method: `ocamllsp/typeSearch`
- params:
    - `TextDocumentPositionParams`: This is an existing interface that includes:
            - `TextDocumentIdentifier`: Specifies the document uri for which the request is sent.
            - `Position`: Specifies the cursor position.
        More details can be found in the [TextDocumentPositionParams - LSP Specification](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentPositionParams).
    - `query`: The search pattern.
    - `limit`: The number of results to return
    - `with_doc`: If to return documentation information or not

## Response

```js
result: TypeSearch | null
export interface TypeSearch {
    type t = Query_protocol.type_search_result list
}
```
- `t`: A list of types that match the query.
    ```
    type Query_protocol.type_search_result =
    {
        "name": string; // The fully qualified name of this result.,
        "typ": string;  // The signature of this result,
        "loc": { // The location of the definition of this result in the source code.
          "end": { "character": int, "line": int },
          "start": { "character": int, "line": int }
        },
        "doc": string // Optional documentation associated with this result.,
        "cost": int; // A numeric value representing the "cost" or distance between this result and the query.
        constructible : string; // A constructible form or template that can be used to invoke this result
    }
    ```
- A response with null result is returned if no entries are found.
