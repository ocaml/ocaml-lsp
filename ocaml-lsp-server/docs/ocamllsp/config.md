# Configuration

The ocamllsp support the folowing configuration. These configurations are sent through the [`didChangeConfiguration`](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didChangeConfiguration) notification.

```ts
interface config {
  /**
  * Enable Extended Hover
  * @default false
  * @since 1.16
  */
  extendedHover: { enable : boolean }

  /**
  * Disable CodeLens
  * @default true
  * @since 1.16
  */
  codelens: { enable : boolean }
}
```
