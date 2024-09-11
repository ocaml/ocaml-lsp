# Polarity Search Request

## Description

This custom request allows clients to perform a polarity-based search at a specific position within a text document based on finding functions or types that match a specific query pattern.

## Server capability

- property name: `handlePolaritySearch`
- property type: `boolean`

## Request

```js
export interface PolaritySearchParams extends TexDocumentPositionParams
{
    query: string;
}
```
- method: `ocamllsp/polaritySearch`
- params:
    - `TextDocumentPositionParams`: This is an existing interface that includes:
            - `TextDocumentIdentifier`: Specifies the document uri for which the request is sent.
            - `Position`: Specifies the cursor position.
        More details can be found in the [TextDocumentPositionParams - LSP Specification](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentPositionParams).
    - `query`: The search pattern.

## Response

```js
result: PolaritySearch | null
export interface PolaritySearch {
    type t = Query_protocol.Compl.entry list
}
```
- `t`: A list of completion entries that match the query.
- A response with null result is returned if no entries are found.
