#### Hover Extended

Alternative hover command providing additional information.

This command is has support for variable verbosity.

```ocaml
type t = int
let x : t = 1
```

With the cursor on the value `x`, a call with a verbosity of 0 or lower would return `t` making it equivalent to a call to `textDocument/hover`. A call with a verbosity of 1 would return `int`.

The management of the verbosity is left to the clients to implement.

##### Client capability

nothing that should be noted

##### Server capability

property name: `handleHoverExtended`
property type: `boolean`

##### Request

- method: `ocamllsp/hoverExtended`
- params:

  ```json
  {
    "textDocument": TextDocumentIdentifier,
    "position": Position,
    "verbosity": integer
  }
  ```

##### Response

The response is similar to the one of `textDocument/hover`

```typescript
	/**
	 * The hover's content
	 */
	contents: MarkedString | MarkedString[] | MarkupContent;
	/**
	 * An optional range is a range inside a text document
	 * that is used to visualize a hover, e.g. by changing the background color.
	 */
	range?: Range;
```
