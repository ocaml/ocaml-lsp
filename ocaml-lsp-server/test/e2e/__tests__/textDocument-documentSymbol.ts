import outdent from "outdent";
import * as LanguageServer from "../src/LanguageServer";
import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";

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

    let result = await query();

    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "deprecated": false,
          "kind": 2,
          "location": Object {
            "range": Object {
              "end": Object {
                "character": 3,
                "line": 6,
              },
              "start": Object {
                "character": 0,
                "line": 3,
              },
            },
            "uri": "file:///test.ml",
          },
          "name": "M",
        },
        Object {
          "containerName": "M",
          "deprecated": false,
          "kind": 12,
          "location": Object {
            "range": Object {
              "end": Object {
                "character": 12,
                "line": 5,
              },
              "start": Object {
                "character": 2,
                "line": 5,
              },
            },
            "uri": "file:///test.ml",
          },
          "name": "n",
        },
        Object {
          "containerName": "M",
          "deprecated": false,
          "kind": 12,
          "location": Object {
            "range": Object {
              "end": Object {
                "character": 19,
                "line": 4,
              },
              "start": Object {
                "character": 2,
                "line": 4,
              },
            },
            "uri": "file:///test.ml",
          },
          "name": "m",
        },
        Object {
          "deprecated": false,
          "kind": 12,
          "location": Object {
            "range": Object {
              "end": Object {
                "character": 20,
                "line": 1,
              },
              "start": Object {
                "character": 0,
                "line": 1,
              },
            },
            "uri": "file:///test.ml",
          },
          "name": "string",
        },
        Object {
          "deprecated": false,
          "kind": 12,
          "location": Object {
            "range": Object {
              "end": Object {
                "character": 12,
                "line": 0,
              },
              "start": Object {
                "character": 0,
                "line": 0,
              },
            },
            "uri": "file:///test.ml",
          },
          "name": "num",
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

    let result = await query();

    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "children": Array [
            Object {
              "children": Array [],
              "deprecated": false,
              "detail": "int",
              "kind": 12,
              "name": "n",
              "range": Object {
                "end": Object {
                  "character": 12,
                  "line": 5,
                },
                "start": Object {
                  "character": 2,
                  "line": 5,
                },
              },
              "selectionRange": Object {
                "end": Object {
                  "character": 12,
                  "line": 5,
                },
                "start": Object {
                  "character": 2,
                  "line": 5,
                },
              },
            },
            Object {
              "children": Array [],
              "deprecated": false,
              "detail": "int -> int -> int",
              "kind": 12,
              "name": "m",
              "range": Object {
                "end": Object {
                  "character": 19,
                  "line": 4,
                },
                "start": Object {
                  "character": 2,
                  "line": 4,
                },
              },
              "selectionRange": Object {
                "end": Object {
                  "character": 19,
                  "line": 4,
                },
                "start": Object {
                  "character": 2,
                  "line": 4,
                },
              },
            },
          ],
          "deprecated": false,
          "kind": 2,
          "name": "M",
          "range": Object {
            "end": Object {
              "character": 3,
              "line": 6,
            },
            "start": Object {
              "character": 0,
              "line": 3,
            },
          },
          "selectionRange": Object {
            "end": Object {
              "character": 3,
              "line": 6,
            },
            "start": Object {
              "character": 0,
              "line": 3,
            },
          },
        },
        Object {
          "children": Array [],
          "deprecated": false,
          "detail": "string",
          "kind": 12,
          "name": "string",
          "range": Object {
            "end": Object {
              "character": 20,
              "line": 1,
            },
            "start": Object {
              "character": 0,
              "line": 1,
            },
          },
          "selectionRange": Object {
            "end": Object {
              "character": 20,
              "line": 1,
            },
            "start": Object {
              "character": 0,
              "line": 1,
            },
          },
        },
        Object {
          "children": Array [],
          "deprecated": false,
          "detail": "int",
          "kind": 12,
          "name": "num",
          "range": Object {
            "end": Object {
              "character": 12,
              "line": 0,
            },
            "start": Object {
              "character": 0,
              "line": 0,
            },
          },
          "selectionRange": Object {
            "end": Object {
              "character": 12,
              "line": 0,
            },
            "start": Object {
              "character": 0,
              "line": 0,
            },
          },
        },
      ]
    `);
  });
});
