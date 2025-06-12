# Configuration

The ocamllsp support the following configurations.

These configurations are sent through the
[`didChangeConfiguration`](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didChangeConfiguration)
notification.

```ts
interface config {
  /**
  * Enable/Disable Extended Hover
  * @default false
  * @since 1.16
  */
  extendedHover: { enable : boolean }

  /**
  * Enable/Disable CodeLens
  * @default false
  * @since 1.16
  */
  codelens: { enable : boolean }

  /**
  * Enable/Disable Dune diagnostics
  * @default true
  * @since 1.18
  */
  duneDiagnostics: { enable : boolean }

  /**
  * Enable/Disable Inlay Hints
  * @default false
  * @since 1.18
  */
  inlayHints: { enable : boolean }

  /**
  * Enable/Disable Syntax Documentation
  * @default false
  * @since 1.18
  */
  syntaxDocumentation: { enable : boolean }
}
```
