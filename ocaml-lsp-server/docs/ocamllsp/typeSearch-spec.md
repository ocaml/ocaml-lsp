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
    doc_format: string;
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
```json
{
    [
        "name": string,
        "typ": string,
        "loc": Range,
        "doc": {
            "value": string,
            "kind": string
        },
        "cost": int,
        "constructible" : string
    ]
}
 ```
- name: The fully qualified name of this result.,
- typ: The signature of this result,
- loc: The location of the definition of this result in the source code.,
- doc: Optional documentation associated with this result.,
- cost: A numeric value representing the "cost" or distance between this result and the query.
- constructible: A constructible form or template that can be used to invoke this result
- A response with null result is returned if no entries are found.
