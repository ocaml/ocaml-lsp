import outdent from "outdent";
import * as LanguageServer from "../src/LanguageServer";
import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";

describe("textDocument/references", () => {
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
    return await languageServer.sendRequest("textDocument/codeLens", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
    });
  }

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
    languageServer.sendNotification("workspace/didChangeConfiguration", {
      settings: {
        codelens: { enable: true },
      },
    });
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
  });

  it("returns codeLens for a module", async () => {
    openDocument(outdent`
      let num = 42
      let string = "Hello"

      module M = struct
        let m a b = a + b
      end
    `);

    let result = await query();

    expect(result).toMatchInlineSnapshot(`
Array [
  Object {
    "command": Object {
      "command": "",
      "title": "int -> int -> int",
    },
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
  },
  Object {
    "command": Object {
      "command": "",
      "title": "string",
    },
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
  },
  Object {
    "command": Object {
      "command": "",
      "title": "int",
    },
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
  },
]
`);
  });
});
