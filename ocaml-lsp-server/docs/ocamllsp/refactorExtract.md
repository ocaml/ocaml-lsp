# Refactor Extract Request

## Description

Provides commands to extract an arbitrary region into a fresh let binding.

## Client Capability

There is no client capability relative to this request.

## Server capability

- property name: `refactorExtract`
- property type: `boolean`

## Request

- method: `ocamllsp/refactorExtract`
- params:

  ```json
  {
    "uri": TextDocumentIdentifier,
    "start": Position,
    "stop": Position,
    "extract_name?": string,
  }
  ```

`start` and `stop` represents the region to be extracted. The `extract_name` parameter allows choosing the name of the generated let binding. If `extract_name` is not specified, a name not taken in the scope is chosen.

## Response

```json
{
  "position": Range,
  "content": string,
  "selection_range": Range
}
```

The result contains the range (`position`) to be replaced (describing the selected region), the output intended to be substituted (`content`) and the range of the identifier of the generated binding (`selection_range`) which allows renaming it easily.
