import outdent from "outdent";
import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";
import * as LanguageServer from "../src/LanguageServer";

describe("textDocument/documentSymbol", () => {
  let languageServer: LanguageServer.LanguageServer;

  function openDocument(source: string) {
    languageServer.sendNotification(
      Protocol.DidOpenTextDocumentNotification.type,
      {
        textDocument: Types.TextDocumentItem.create(
          "file:///test.ml",
          "ocaml",
          0,
          source,
        ),
      },
    );
  }

  async function query() {
    return await languageServer.sendRequest("textDocument/documentSymbol", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
    });
  }

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
  });

  it("returns a list of symbol infos", async () => {
    languageServer = await LanguageServer.startAndInitialize();
    openDocument(outdent`
      let num = 42
      let string = "Hello"

      module M = struct
        let m a b = a + b
        let n = 32
      end
    `);

    const result = await query();

    expect(result).toMatchInlineSnapshot(`
[
  {
    "kind": 13,
    "location": {
      "range": {
        "end": {
          "character": 12,
          "line": 0,
        },
        "start": {
          "character": 0,
          "line": 0,
        },
      },
      "uri": "file:///test.ml",
    },
    "name": "num",
  },
  {
    "kind": 13,
    "location": {
      "range": {
        "end": {
          "character": 20,
          "line": 1,
        },
        "start": {
          "character": 0,
          "line": 1,
        },
      },
      "uri": "file:///test.ml",
    },
    "name": "string",
  },
  {
    "kind": 2,
    "location": {
      "range": {
        "end": {
          "character": 3,
          "line": 6,
        },
        "start": {
          "character": 0,
          "line": 3,
        },
      },
      "uri": "file:///test.ml",
    },
    "name": "M",
  },
  {
    "containerName": "M",
    "kind": 12,
    "location": {
      "range": {
        "end": {
          "character": 19,
          "line": 4,
        },
        "start": {
          "character": 2,
          "line": 4,
        },
      },
      "uri": "file:///test.ml",
    },
    "name": "m",
  },
  {
    "containerName": "M",
    "kind": 13,
    "location": {
      "range": {
        "end": {
          "character": 12,
          "line": 5,
        },
        "start": {
          "character": 2,
          "line": 5,
        },
      },
      "uri": "file:///test.ml",
    },
    "name": "n",
  },
]
`);
  });

  it("returns a hierarchy of symbols", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      capabilities: {
        textDocument: {
          documentSymbol: {
            hierarchicalDocumentSymbolSupport: true,
          },
          moniker: {},
        },
      },
    });
    openDocument(outdent`
      let num = 42
      let string = "Hello"

      module M = struct
        let m a b = a + b
        let n = 32
      end
    `);

    const result = await query();

    expect(result).toMatchInlineSnapshot(`
[
  {
    "children": [],
    "kind": 13,
    "name": "num",
    "range": {
      "end": {
        "character": 12,
        "line": 0,
      },
      "start": {
        "character": 0,
        "line": 0,
      },
    },
    "selectionRange": {
      "end": {
        "character": 7,
        "line": 0,
      },
      "start": {
        "character": 4,
        "line": 0,
      },
    },
  },
  {
    "children": [],
    "kind": 13,
    "name": "string",
    "range": {
      "end": {
        "character": 20,
        "line": 1,
      },
      "start": {
        "character": 0,
        "line": 1,
      },
    },
    "selectionRange": {
      "end": {
        "character": 10,
        "line": 1,
      },
      "start": {
        "character": 4,
        "line": 1,
      },
    },
  },
  {
    "children": [
      {
        "children": [],
        "kind": 12,
        "name": "m",
        "range": {
          "end": {
            "character": 19,
            "line": 4,
          },
          "start": {
            "character": 2,
            "line": 4,
          },
        },
        "selectionRange": {
          "end": {
            "character": 7,
            "line": 4,
          },
          "start": {
            "character": 6,
            "line": 4,
          },
        },
      },
      {
        "children": [],
        "kind": 13,
        "name": "n",
        "range": {
          "end": {
            "character": 12,
            "line": 5,
          },
          "start": {
            "character": 2,
            "line": 5,
          },
        },
        "selectionRange": {
          "end": {
            "character": 7,
            "line": 5,
          },
          "start": {
            "character": 6,
            "line": 5,
          },
        },
      },
    ],
    "kind": 2,
    "name": "M",
    "range": {
      "end": {
        "character": 3,
        "line": 6,
      },
      "start": {
        "character": 0,
        "line": 3,
      },
    },
    "selectionRange": {
      "end": {
        "character": 8,
        "line": 3,
      },
      "start": {
        "character": 7,
        "line": 3,
      },
    },
  },
]
`);
  });
});
