# Complete Prefix At Position

## Description

Requests completions that could come after a given prefix at a given position in the
buffer.  The prefix may or may not actually appear before that position in the buffer.

This is the information that Emacs completion-at-point wants to receive in order to enable
the full range of Emacs completion tools and facilities.  This replaces how merlin-cap.el
invoked `merlin complete-prefix -position ... -prefix ...` for every completion request.

The default LSP `textDocument/completion` cannot be used in a way that is equivalent to
completing a prefix at a position.  In particular, there is no prefix in the parameters.

In the case of a completion context of completing the arguments to a function call, return
the function arguments separately instead of squashing them into the list of completions.
`textDocument/completion` does the latter for LSP protocol compatibility.

## Client capability

There is no client capability relevant to this request.

## Server capability

property name: `handleCompletePrefixAtPos`

property type: `boolean`

## Request

- method: `ocamllsp/completePrefixAtPos`
- params:

```json
{
    "textDocument": TextDocumentIdentifier,
    "position": Position,
    "prefix": string,
}
```

- `textDocument`: is the reference of the current document
- `position`: is the position in that document at which completions are requested
- `prefix`: is a string specifying a prefix shared by all valid completions.  This can be
  an empty string, typically in the case that the user has not typed anything yet.

## Response

- result:

```json
{
    "entries": null | CompletionItem[],
    "context"?: [ "application", { "labels": FunctionLabelItem[] } ]
}
```

`context` is omitted unless the position is in a function application.

where a `FunctionLabelItem` has the form

```json
{
    "name": string,
    "type": string
}
```

We don't need the `CompletionList` supported by `textDocument/completion`.
