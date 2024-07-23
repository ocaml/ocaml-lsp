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
- method : `ocamllsp/getDocumentation`
- params :
    - `TextDocumentPositionParams`: A record which contains the `TextDocumentIdentifier` and `Position`.
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
