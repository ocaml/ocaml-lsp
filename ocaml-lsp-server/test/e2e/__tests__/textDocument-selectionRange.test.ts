import outdent from "outdent";
import * as LanguageServer from "../src/LanguageServer";
import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";

describe("textDocument/selectionRange", () => {
  let languageServer: LanguageServer.LanguageServer;

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
  });

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

  async function selectionRange(positions: Types.Position[]) {
    return await languageServer.sendRequest(
      Protocol.SelectionRangeRequest.type,
      {
        textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
        positions: positions,
      },
    );
  }

  it("returns a selection range for modules", async () => {
    openDocument(outdent`
      let foo a b =
        let min_ab = min a b in
        let max_ab = max a b in
        min_ab * max_ab
        `);

    let result = await selectionRange([Types.Position.create(1, 17)]);
    expect(result).toMatchInlineSnapshot(`
Array [
  Object {
    "parent": Object {
      "parent": Object {
        "parent": Object {
          "parent": Object {
            "parent": Object {
              "parent": Object {
                "parent": Object {
                  "range": Object {
                    "end": Object {
                      "character": 17,
                      "line": 3,
                    },
                    "start": Object {
                      "character": 0,
                      "line": 0,
                    },
                  },
                },
                "range": Object {
                  "end": Object {
                    "character": 17,
                    "line": 3,
                  },
                  "start": Object {
                    "character": 0,
                    "line": 0,
                  },
                },
              },
              "range": Object {
                "end": Object {
                  "character": 17,
                  "line": 3,
                },
                "start": Object {
                  "character": 0,
                  "line": 0,
                },
              },
            },
            "range": Object {
              "end": Object {
                "character": 17,
                "line": 3,
              },
              "start": Object {
                "character": 8,
                "line": 0,
              },
            },
          },
          "range": Object {
            "end": Object {
              "character": 17,
              "line": 3,
            },
            "start": Object {
              "character": 2,
              "line": 1,
            },
          },
        },
        "range": Object {
          "end": Object {
            "character": 22,
            "line": 1,
          },
          "start": Object {
            "character": 2,
            "line": 1,
          },
        },
      },
      "range": Object {
        "end": Object {
          "character": 22,
          "line": 1,
        },
        "start": Object {
          "character": 15,
          "line": 1,
        },
      },
    },
    "range": Object {
      "end": Object {
        "character": 18,
        "line": 1,
      },
      "start": Object {
        "character": 15,
        "line": 1,
      },
    },
  },
]
`);
  });
});
