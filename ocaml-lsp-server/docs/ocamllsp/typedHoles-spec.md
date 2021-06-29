# Typed Holes Request

## Description

### A note on stability: 

OCaml-LSP does not guarantee stability for this custom request, meaning the core 
contributors may change or remove this custom request, as they see necessary.

### What typed holes are

Merlin has a concept of "typed holes" that are syntactically represented as `_`. Files
that incorporate typed holes are not considered valid OCaml, but Merlin and OCaml-LSP
support them. One example when such typed holes can occur is when on "destructs" a value,
e.g., destructing `(Some 1)` will generate code `match Some 1 with Some _ -> _ | None ->
_`. While the first underscore is a valid "match-all"/wildcard pattern, the rest of
underscores are typed holes.

### Why this custom request needed

It is reasonable that user wants to jump around such typed holes to be able to edit them.
This custom request allows clients to know where these holes are and enable jumping around
them.

## Client capability

nothing that should be noted

## Server capability

property name: `handleTypedHoles`

property type: `boolean`

## Request

- method: `ocamllsp/typedHoles`
- params:

```json
{
  "uri": DocumentUri,
}
```

## Response

- result: `Range[]`
  - empty array if no holes found in the file at the given `URI`
- error: code and message set in case an exception happens during the
  `ocamllsp/typedHoles` request.
  - in case of any errors in finding holes in the file, the handler throws an exception,
    which is returned from the language server.
