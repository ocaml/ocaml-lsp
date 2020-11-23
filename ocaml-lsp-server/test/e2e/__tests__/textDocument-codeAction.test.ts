import outdent from "outdent";
import * as LanguageServer from "../src/LanguageServer";

import * as Types from "vscode-languageserver-types";

describe("textDocument/codeAction", () => {
  let languageServer = null;

  async function openDocument(source, name) {
    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///" + name,
        "ocaml",
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

  async function codeAction(name, start, end) {
    return await languageServer.sendRequest("textDocument/codeAction", {
      textDocument: Types.TextDocumentIdentifier.create("file:///" + name),
      context: { diagnostics: [] },
      range: { start, end },
    });
  }

  it("can destruct sum types", async () => {
    await openDocument(
      outdent`
type t = Foo of int | Bar of bool

let f (x : t) = x
`,
      "test.ml",
    );
    let start = Types.Position.create(2, 16);
    let end = Types.Position.create(2, 17);
    let actions = await codeAction("test.ml", start, end);
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

  it("can infer module interfaces", async () => {
    await openDocument(
      outdent`
type t = Foo of int | Bar of bool

let f (x : t) = x
`,
      "test.ml",
    );
    await openDocument("", "test.mli");
    let start = Types.Position.create(0, 0);
    let end = Types.Position.create(0, 0);
    let actions = await codeAction("test.mli", start, end);
    expect(actions).toMatchObject([
      {
        edit: {
          changes: {
            "file:///test.mli": [
              {
                newText: "type t = Foo of int | Bar of bool\nval f : t -> t\n",
                range: {
                  end: {
                    character: 1,
                    line: 1,
                  },
                  start: {
                    character: 1,
                    line: 1,
                  },
                },
              },
            ],
          },
        },
        kind: "inferred_intf",
        title: "Insert inferred interface",
      },
    ]);
  });
});
