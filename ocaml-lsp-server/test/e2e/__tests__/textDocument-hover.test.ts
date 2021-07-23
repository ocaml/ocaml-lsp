import outdent from "outdent";
import * as LanguageServer from "./../src/LanguageServer";

import * as Types from "vscode-languageserver-types";

describe("textDocument/hover", () => {
  let languageServer: LanguageServer.LanguageServer;

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
  });

  it("returns type inferred under cursor", async () => {
    languageServer = await LanguageServer.startAndInitialize();
    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test.ml",
        "ocaml",
        0,
        "let x = 1\n",
      ),
    });

    let result = await languageServer.sendRequest("textDocument/hover", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
      position: Types.Position.create(0, 4),
    });

    expect(result).toMatchObject({
      contents: { kind: "plaintext", value: "int" },
      range: {
        end: { character: 5, line: 0 },
        start: { character: 4, line: 0 },
      },
    });
  });

  it("returns type inferred under cursor (markdown formatting)", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      capabilities: {
        textDocument: {
          hover: {
            dynamicRegistration: true,
            contentFormat: ["markdown", "plaintext"],
          },
          moniker: {},
        },
      },
    });
    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test.ml",
        "ocaml",
        0,
        "let x = 1\n",
      ),
    });

    let result = await languageServer.sendRequest("textDocument/hover", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
      position: Types.Position.create(0, 4),
    });

    expect(result).toMatchObject({
      contents: { kind: "markdown", value: "```ocaml\nint\n```" },
      range: {
        end: { character: 5, line: 0 },
        start: { character: 4, line: 0 },
      },
    });
  });

  it("returns type inferred under cursor with documentation", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      capabilities: {
        textDocument: {
          hover: {
            dynamicRegistration: true,
            contentFormat: ["markdown", "plaintext"],
          },
          moniker: {},
        },
      },
    });
    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test.ml",
        "ocaml",
        0,
        outdent`
        (** This function has a nice documentation *)
        let id x = x

        `,
      ),
    });

    let result = await languageServer.sendRequest("textDocument/hover", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
      position: Types.Position.create(1, 4),
    });

    expect(result).toMatchObject({
      contents: {
        kind: "markdown",
        value: outdent`
          \`\`\`ocaml
          'a -> 'a
          \`\`\`
          ---
          This function has a nice documentation
          `,
      },
    });
  });

  it("returns type inferred under cursor with documentation with tags (markdown formatting)", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      capabilities: {
        textDocument: {
          hover: {
            dynamicRegistration: true,
            contentFormat: ["markdown", "plaintext"],
          },
          moniker: {},
        },
      },
    });
    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test.ml",
        "ocaml",
        0,
        outdent`
        (** This function has a nice documentation.

            It performs division of two integer numbers.

            @param x dividend
            @param divisor

            @return {i quotient}, i.e. result of division
            @raise Division_by_zero raised when divided by zero

            @see <https://en.wikipedia.org/wiki/Arithmetic#Division_(%C3%B7,_or_/)> article
            @see 'arithmetic.ml' for more context

            @since 4.0.0
            @before 4.4.0

            @deprecated use [(/)]

            @version 1.0.0
            @author John Doe *)
        let div x y =
          x / y

        `,
      ),
    });

    let result = await languageServer.sendRequest("textDocument/hover", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
      position: Types.Position.create(20, 4),
    });

    expect(result).toMatchObject({
      contents: {
        kind: "markdown",
        value: outdent`
          \`\`\`ocaml
          int -> int -> int
          \`\`\`
          ---
          This function has a nice documentation.

          It performs division of two integer numbers.
          * * *
          ***@param*** \`x\` dividend

          ***@param*** divisor

          ***@return*** *quotient*, i.e. result of division

          ***@raise*** \`Division_by_zero\` raised when divided by zero

          ***@see*** [link](https://en.wikipedia.org/wiki/Arithmetic#Division_(%C3%B7,_or_/)) article

          ***@see*** \`arithmetic.ml\` for more context

          ***@since*** \`4.0.0\`

          ***@before*** \`4.4.0\`

          ***@deprecated*** use \`(/)\`

          ***@version*** \`1.0.0\`

          ***@author*** John Doe
          `,
      },
    });
  });

  it("returns good type when cursor is between values", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      capabilities: {
        textDocument: {
          hover: {
            dynamicRegistration: true,
            contentFormat: ["markdown", "plaintext"],
          },
          moniker: {},
        },
      },
    });
    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test.ml",
        "ocaml",
        0,
        outdent`
          let f i f = float_of_int i +. f
          let i = 10
          let f = 10.
          let sum = f i f
       `,
      ),
    });

    let result = await languageServer.sendRequest("textDocument/hover", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
      position: Types.Position.create(3, 13),
    });

    expect(result).toMatchObject({
      contents: {
        kind: "markdown",
        value: "```ocaml\nint\n```",
      },
      range: {
        start: { character: 12, line: 3 },
        end: { character: 13, line: 3 },
      },
    });
  });

  it("regression test for #343", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      capabilities: {
        textDocument: {
          hover: {
            dynamicRegistration: true,
            contentFormat: ["markdown", "plaintext"],
          },
          moniker: {},
        },
      },
    });
    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test.ml",
        "ocaml",
        0,
        outdent`
          type t = s
          and s = string
          type 'a fib = ('a -> unit) -> unit
       `,
      ),
    });

    let hover1 = await languageServer.sendRequest("textDocument/hover", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
      position: Types.Position.create(1, 4),
    });

    expect(hover1).toMatchInlineSnapshot(`
      Object {
        "contents": Object {
          "kind": "markdown",
          "value": "\`\`\`ocaml
      type s = t
      \`\`\`",
        },
        "range": Object {
          "end": Object {
            "character": 14,
            "line": 1,
          },
          "start": Object {
            "character": 0,
            "line": 1,
          },
        },
      }
    `);

    let hover2 = await languageServer.sendRequest("textDocument/hover", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
      position: Types.Position.create(2, 9),
    });

    expect(hover2).toMatchInlineSnapshot(`
      Object {
        "contents": Object {
          "kind": "markdown",
          "value": "\`\`\`ocaml
      type 'a fib = ('a -> unit) -> unit
      \`\`\`",
        },
        "range": Object {
          "end": Object {
            "character": 34,
            "line": 2,
          },
          "start": Object {
            "character": 0,
            "line": 2,
          },
        },
      }
    `);
  });

  it("regression test for #403", async () => {
    languageServer = await LanguageServer.startAndInitialize();
    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test.ml",
        "ocaml",
        0,
        outdent`
type foo = int

let x : foo = 1
`,
      ),
    });

    let result = await languageServer.sendRequest("textDocument/hover", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
      position: Types.Position.create(2, 4),
    });

    expect(result).toMatchInlineSnapshot(`
      Object {
        "contents": Object {
          "kind": "plaintext",
          "value": "foo",
        },
        "range": Object {
          "end": Object {
            "character": 5,
            "line": 2,
          },
          "start": Object {
            "character": 4,
            "line": 2,
          },
        },
      }
    `);
  });

  it("FIXME: reproduce [#344](https://github.com/ocaml/ocaml-lsp/issues/344)", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      capabilities: {
        textDocument: {
          hover: {
            dynamicRegistration: true,
            contentFormat: ["markdown", "plaintext"],
          },
          moniker: {},
        },
      },
    });

    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test.ml",
        "ocaml",
        0,
        // the empty space below is necessary to reproduce the bug
        outdent`
























        let k = ()
        let m = List.map
        `,
      ),
    });

    // here we see that all is ok
    let hoverOverK = await languageServer.sendRequest("textDocument/hover", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
      position: Types.Position.create(24, 4),
    });

    expect(hoverOverK).toMatchInlineSnapshot(`
      Object {
        "contents": Object {
          "kind": "markdown",
          "value": "\`\`\`ocaml
      unit
      \`\`\`",
        },
        "range": Object {
          "end": Object {
            "character": 5,
            "line": 24,
          },
          "start": Object {
            "character": 4,
            "line": 24,
          },
        },
      }
    `);

    // we trigger the bug
    let autocompleteForListm = await languageServer.sendRequest(
      "textDocument/hover",
      {
        textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
        position: Types.Position.create(25, 15),
      },
    );

    let buggedHoverOverK = await languageServer.sendRequest(
      "textDocument/hover",
      {
        textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
        position: Types.Position.create(24, 4),
      },
    );

    // now the same hover as before comes with unrelated documentation
    expect(buggedHoverOverK).toMatchInlineSnapshot(`
      Object {
        "contents": Object {
          "kind": "markdown",
          "value": "\`\`\`ocaml
      unit
      \`\`\`",
        },
        "range": Object {
          "end": Object {
            "character": 5,
            "line": 24,
          },
          "start": Object {
            "character": 4,
            "line": 24,
          },
        },
      }
    `);
  });
});
