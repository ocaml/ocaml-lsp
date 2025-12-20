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
  * Enable/Disable Standard Hover
  * @default true
  * @since 1.21
  */
  standardHover: { enable : boolean }

  codelens: {
    /**
    * Enable/Disable CodeLens
    * @default false
    * @since 1.16
    */
    enable : boolean,

    /**
    * Enable CodeLens for nested let bindings
    * @default false
    * @since 1.25
    */
    forNestedBindings : boolean
  }

  /**
  * Enable/Disable Dune diagnostics
  * @default true
  * @since 1.18
  */
  duneDiagnostics: { enable : boolean }

  inlayHints: {
    /**
    * Enable/Disable Inlay Hints for pattern variables
    * @default false
    * @since 1.23
    */
    hintPatternVariables : boolean

    /**
    * Enable/Disable Inlay Hints for let bindings
    * @default false
    * @since 1.23
    */
    hintLetBindings : boolean

    /**
    * Enable/Disable Inlay Hints for function params
    * @default false
    * @since 1.23
    */
    hintFunctionParams : boolean
  }

  /**
  * Enable/Disable Syntax Documentation
  * @default false
  * @since 1.18
  */
  syntaxDocumentation: { enable : boolean }

  /**
  * Enable/Disable Merlin Jump code actions
  * @default false
  * @since 1.19
  */
  merlinJumpCodeActions: { enable : boolean }

  /**
  * Enable/Disable Shorten Merlin Diagnostics
  * @default false
  * @since 1.23
  */
  shortenMerlinDiagnostics: { enable : boolean }
}
```
