# Configuration

The ocamllsp support the folowing configurations.

These configurations are sent through the
[`didChangeConfiguration`](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didChangeConfiguration)
notification.

```ts
interface config {
  /**
  * Provide more information on `textDocumeŧ/hover` request
  * @since 1.16
  */
  extendedHover: {
    /**
    * Enable/Disable Extended Hover
    * @default false
    */
    enable: boolean,

    /**
    * The level of verbosity
    * @default 0
    */
    verbosity?: uinteger
  }

  /**
  * Enable/Disable CodeLens
  * @default false
  * @since 1.16
  */
  codelens: { enable : boolean }
}
```

## Extended Hover

Alternative hover config providing additional information.

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
