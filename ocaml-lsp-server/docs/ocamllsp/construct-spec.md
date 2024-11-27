# Construct Request

## Description

Provides commands to fill typed holes (`_`). Such holes sometimes
appear in the result of other commands like `destruct` and can also be inserted
manually in the source. The command is already accessible via a completion hook,
however, in certain situations, invoking `construct` on a hole via a request
allows more control.

## Client Capability

There is no client capability relative to this request.

## Server capability

- property name: `handleConstruct`
- property type: `boolean`

## Request

- method: `ocamllsp/construct`
- params:

  ```json
  {
    "uri": TextDocumentIdentifier,
    "position": Position
    "depth?": uinteger (default value: 0)
    "withValues?": <"local" | "none">,
  }
  ```

The `depth` parameter allows to recursively construct terms. Note that
when `depth > 1` partial results of inferior depth will not be
returned. The `withValues` parameter enables the use of values from
the environment (`local`) or not (`none`), It defaults to `none`.

## Response

```json
{
  "position": Range,
  "result": string[]
}
```

The result contains the range (`position`) to be replaced (describing the hole)
and the list of possible substitution values (`result`).
