# Next Hole Request

Merlin has a concept of "typed holes" that are syntactically represented as `_`. Files
that incorporate typed holes are not considered valid OCaml, but Merlin and OCaml-LSP
support them. One example when such typed holes can occur is when on "destructs" a value,
e.g., destructing `(Some 1)` will generate code `match Some 1 with Some _ -> _ | None ->
_`. While the first underscore is a valid "match-all"/wildcard pattern, the rest of
underscores are typed holes.

It is reasonable that user wants to jump to such typed holes to be able to edit them. One
should note that at the time of this writing, there is work on Merlin, which allows to
generate real code for such typed holes automatically based on the type of the hole and
available values in the current context.

## Client capability

nothing that should be noted

## Server capability

property name: `handleNextHole`

property type: `boolean`

## Request

- method: `ocamllsp/nextHole`
- params:

```json
{
  "uri": DocumentUri,
  "position": Position
}
```

## Response

- result: `Range | Null`
  - `Range` if the next hole was found
  - `Null` if there are no holes in the file
- error: code and message set in case an exception happens during the
  `ocamllsp/nextHole` request.
  - in case of any errors in finding a next hole in a file, the handler throws an
    exception, which is returned from the language server.
