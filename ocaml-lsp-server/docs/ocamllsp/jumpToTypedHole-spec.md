# Jump To Typed Holes Requet

### Description

Returns the next or previous typed hole at a given position (included
in a range or not).


### Why this custom request needed

Reduces the need for editor-side logic and works well with the
expression construct command. For example, constructing a value of
type `int option` constructs the following expression:
`Some _`, coupled with typed hole navigation, you can move the cursor
directly over the generated hole.


### A note on stability: 

> OCaml-LSP does not guarantee stability for this custom request,
> meaning the core contributors may change or remove this custom
> request, as they see necessary.

## Client capability

N/A

## Server capability

- property name: `handleJumpToTypedHole`
- property type: `boolean`

## Request

- method: `ocamllsp/jumpToTypedHole`
- params:

```json
{
  "uri": TextDocumentIdentifier,
  "position": Position,
  "direction": <"next"|"prev">,
  "range?": Range
}
```

If a `range` is given, it will only select holes present in the given
range.

## Response

```json
Range | null
```

Returns the next or previous typed hole at a given position. An
optional range can be used to restrict the search.
