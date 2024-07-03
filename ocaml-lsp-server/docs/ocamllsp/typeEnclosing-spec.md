# Type Enclosing Request

## Description

Merlin has a concept of `type enclosing` that gets the type of ident under the
cursor. It will highlight the ident and display its type. You can climb the
typed-tree and display the type of bigger expressions surrounding the cursor. In
order to keep the request stateless, the manipulation related to growing or
shrinking enclosings is delegated to the client. This request allows to request
type enclosing under the cursor and then its surrounding enclosings.

## Client capability

There is no client capability relative to this request.

## Server capability

- property name: `handleTypeEnclosing`
- property type: `boolean`

## Request

- method: `ocamllsp/typeEnclosing`
- params:

  ```json
  {
    "uri": TextDocumentIdentifier,
    "at": (Position | Range),
    "index": uinteger,
    "verbosity?": uinteger,
  }
  ```

  - `index` can be used to print only one type information. This is useful to query
    the types lazily: normally, Merlin would return the signature of all enclosing
    modules, which can be very expensive.
  - `verbosity` determines the number of expansions of aliases in answers.
  - `at` :
    - if a `Position` is given, it will returns all enclosing around the position
    - if a `Range` is given, only enclosings that contain the range
    `[range.start; range.end]` will be included in the answer


## Response

```json
{
  "enclosings": Range[],
  "index": uinteger,
  "type": string
}
```

- `enclosings`: The surrounding enclosings
- `index` The index of the provided type result: the index corresponds to a
  zero-indexed enclosing in the `enclosings`' array. It is the same value as the
  one provided in this request's `TypeEnclosingParams`
- `type`: The type of the enclosing `enclosings[index]` as a raw string
