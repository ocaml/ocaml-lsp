# Documentation Request

## Description

Merlin has a command `document` that gets `odoc` documentation for symbols based on a cursor position or an optional identifier. This request allows documentation to be gotten using a classic Merlin workflow.

## Client capability

```js
export interface GetDocClientCapabilities {
	contentFormat: MarkupKind[];
}
```
- `contentFormat`: Client supports the following content formats if the content property refers to a `literal of type MarkupContent`. The order describes the preferred format of the client.

## Request

```js
export interface GetDocParams extends TextDocumentPositionParams
{
    identifier?: string;
    contentFormat?:MarkupKind;
}
```
- `position`: The position of the cursor.
- `identifier`: An optional identifier. If provided, documentation for this ident is looked up from the environment at the given position. Else the server will look for the documentation of the identifier under the cursor.
- `contentFormat`: Optionally override the result's format. Could be `Plaintext` or `Markdown`.

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
