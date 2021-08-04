#### Wrapping AST Node

(Could also be named `Enclosing AST Node`)

Returns the range of the (typed) AST node enclosing the cursor at given position in the document with the given URI.

Let's see some examples (`<n>` is
cursor position for example number `n`)

```ocaml
let k = <1> 1
<2>
module M = struct<3>
  let a =
    let <4> b = 1 in
    b + 1
end
```

| Your cursor | Evaluated expression        | Rationale                                                                                        |
| ----------- | --------------------------- | ------------------------------------------------------------------------------------------------ |
| <1>         | `let k = 1`                 | Toplevel expression under cursor                                                                 |
| <2>         | whole code block            | Cursor is in-between, so whole "file" is evaluated                                               |
| <3>         | `module M = ... end`        | Cursor is on the module definition                                                               |
| <4>         | `let a = let b = 1 in b + 1 | "Toplevel" expression for the cursor; it's used because it's "closer" than the module definition |

The returned range can be a null when the document is empty.

The document URI in the request has to be open before sending a the request. If the file cannot be found in the document store (i.e., wasn't previously opened), will return an error.

Note: stability of this custom request is not guaranteed. Talk to the maintainers if you want to depend on it.

##### Client capability

nothing that should be noted

##### Server capability

property name: `handleWrappingAstNode`
property type: `boolean`

##### Request

- method: `ocamllsp/wrappingAstNode`
- params:

  ```json
  {
    "uri": DocumentUri,
    "position": Position
  }
  ```

##### Response

- result: `Range | null`
- error: code and message set in case an exception happens during the processing of the request.
