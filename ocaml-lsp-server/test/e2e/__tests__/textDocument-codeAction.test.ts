import outdent from "outdent";
import * as LanguageServer from "../src/LanguageServer";

import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";

describe("textDocument/codeAction", () => {
  let languageServer = null;

  async function openDocument(source) {
    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test.ml",
        "txt",
        0,
        source,
      ),
    });
  }

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
    languageServer = null;
  });

  async function codeAction(start, end) {
    return await languageServer.sendRequest("textDocument/codeAction", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
      context: { diagnostics: [] },
      range: { start, end },
    });
  }

  it("can destruct sum types", async () => {
    await openDocument(outdent`
type t = Foo of int | Bar of bool

let f (x : t) = x
`);
    let start = Types.Position.create(2, 16);
    let end = Types.Position.create(2, 17);
    let actions = await codeAction(start, end);
    expect(actions).toMatchObject([
      {
        edit: {
          changes: {
            "file:///test.ml": [
              {
                newText: "match x with | Foo _ -> (??) | Bar _ -> (??)",
                range: {
                  end: {
                    character: 17,
                    line: 2,
                  },
                  start: {
                    character: 16,
                    line: 2,
                  },
                },
              },
            ],
          },
        },
        kind: "destruct",
        title: "Destruct",
      },
    ]);
  });
});
