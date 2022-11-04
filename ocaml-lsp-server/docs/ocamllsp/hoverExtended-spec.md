#### Hover Extended

Alternative hover command providing additional information.

This command has support for variable verbosity.

```ocaml
type t = int
let x : t = 1
```

With the cursor on the value `x`, a call with a verbosity of 0 or lower would
return `t` making it equivalent to a call to `textDocument/hover`. A call with
a verbosity of 1 would return `int`.

When the verbosity is omitted, the server picks a number based on previous
calls. It starts with 0. Further calls at the same position will improve the
verbosity of the displayed type, by expanding aliases. If the position changes,
the verbosity goes back to 0. This behavior is similar to `type-enclosing` in
merlin.

##### Client capability

nothing that should be noted

##### Server capability

property name: `handleHoverExtended`
property type: `boolean`

##### Request

- method: `ocamllsp/hoverExtended`
- params:

  ```typescript
  {
    textDocument: TextDocumentIdentifier,
    position: Position,
    verbosity?: integer
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
