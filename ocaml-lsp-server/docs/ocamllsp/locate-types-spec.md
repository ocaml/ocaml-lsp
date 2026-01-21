# Locate Types Request

## Description

A custom query that returns a tree representing all the types that
make up the type of the expression under the cursor. For example, in
the following expression: 

```ocaml
type a
type b
type c
type 'a one_arg
type ('a, 'b) two_arg

let f : a -> (a, b * c) two_arg = fun x -> assert false
```

On `f`, the request will returns a tree locating `a`, `b`, `c` and `two_arg`.

## Client Capability

There is no client capability relative to this request.

## Server Capability 

- propert name: `handleLocateTypes`
- property type: `boolean`

## Request

- method: `ocamllsp/locateTypes`
- params:

  ```json
  {
    "uri": TextDocumentIdentifier,
    "position": Position,
  }
  ```

## Response

```json
result = {
  "data": { "kind": "arrow" 
                  | "tuple" 
                  | "object" 
                  | "poly-variant" 
                  | "type_ref"
           },
  "children": List of result
}
```

The `data.kind` field may change the structure of the `data` field in
the case of `type-ref`:

```json
{ 
  "ty": string,
  "result": { "kind": "found"
                    | "builtin"
                    | "not-in-env"
                    | "file-not-found"
                    | "not-found"
            }
}
```

The `result.kind` may also change the structure of the `result` field:

- `found`: `{kind: "found", "has_uri": bool, "uri?": DocumentUri, "position": Position}`

- `builtin`: `{kind: "builtin", "type": string}`
- `not-in-env`: `{kind: "not-in-env", "type": string}`
- `file-not-found`: `{kind: "file-not-found", "uri": DocumentUri}`
- `not-found`: `{kind: "not-found", "type?": string}`
