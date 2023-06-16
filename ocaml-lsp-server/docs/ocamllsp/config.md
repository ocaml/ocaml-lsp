# Configuration

The ocamllsp support the folowing configurations.

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
}
```
