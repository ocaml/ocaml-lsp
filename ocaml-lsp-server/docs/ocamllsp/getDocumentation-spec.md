# Documentation Request

## Description

 This custom request allows `odoc` documentation to be gotten without using Hover.

## Client capability

```js
export interface GetDocClientCapabilities {
	contentFormat: MarkupKind[];
}
```
- `contentFormat`: Client supports the following content formats if the content property refers to a `literal of type MarkupContent`. The order describes the preferred format of the client.

## Server capability

- property name: `handleDocumentation`
- property type: `boolean`


## Request

```js
export interface GetDocParams extends TextDocumentPositionParams
{
    identifier?: string;
    contentFormat?:MarkupKind;
}
```
- method: `ocamllsp/getDocumentation`
- params:
    - `TextDocumentPositionParams`: This is an existing interface that includes:
        - `TextDocumentIdentifier`: Specifies the document for which the request is sent. It includes a uri property that points to the document.
        - `Position`: Specifies the position in the document for which the documentation is requested. It includes line and character properties.
    More details can be found in the [TextDocumentPositionParams - LSP Specification](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentPositionParams).
    - `identifier` (Optional): A string representing an identifier for which the documentation is requested. If provided, the documentation lookup will be specifically for this identifier in the context of the position in the document. If omitted, the server will automatically fetch the documentation for the identifier currently under the cursor at the given position.
    - `contentFormat` (Optional): This parameter specifies the desired format for the returned documentation content. It can be either:
        - `Plaintext`: The documentation will be returned in plain text format.
        - `Markdown`: The documentation will be returned in Markdown format.
    The type `MarkupKind` typically supports these two formats, as specified in the [MarkupKind - LSP protocol](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#markupContent).

## Response

```js
result: GetDoc | null
export interface GetDoc {
    doc: MarkupContent;
}

```
- `doc`: The documentation found
- A response with null result is returned if the identifier doesn't have documentation.
- An error is returned if the identifier is invalid.
