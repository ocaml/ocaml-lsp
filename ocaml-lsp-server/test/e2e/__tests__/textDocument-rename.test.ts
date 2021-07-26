import outdent from "outdent";
import * as LanguageServer from "../src/LanguageServer";

import * as Types from "vscode-languageserver-types";

describe("textDocument/rename", () => {
  let languageServer = null;

  async function openDocument(source: string) {
    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test.ml",
        "ocaml",
        0,
        source,
      ),
    });
  }

  async function query(position: Types.Position, newNameOpt?: string) {
    let newName = newNameOpt ? newNameOpt : "new_num";
    return await languageServer.sendRequest("textDocument/rename", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
      position,
      newName,
    });
  }

  async function query_prepare(position: Types.Position) {
    return await languageServer.sendRequest("textDocument/prepareRename", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
      position,
    });
  }

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
    languageServer = null;
  });

  it("can reject invalid rename request", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      capabilities: {
        workspace: { workspaceEdit: { documentChanges: false } },
      },
    });

    await openDocument(outdent`
      let num = 42
      let num = num + 13
      let num2 = num
    `);

    let result = await query_prepare(Types.Position.create(0, 1));
    expect(result).toBeNull();
  });

  it("allows valid rename request", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      capabilities: {
        workspace: { workspaceEdit: { documentChanges: false } },
      },
    });

    await openDocument(outdent`
      let num = 42
      let num = num + 13
      let num2 = num
    `);

    let result = await query_prepare(Types.Position.create(0, 4));
    expect(result).toMatchObject({
      start: { line: 0, character: 4 },
      end: { line: 0, character: 7 },
    });
  });

  it("rename value in a file without documentChanges capability", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      capabilities: {
        workspace: { workspaceEdit: { documentChanges: false } },
      },
    });

    await openDocument(outdent`
      let num = 42
      let num = num + 13
      let num2 = num
    `);

    let result = await query(Types.Position.create(0, 4));

    expect(result).toMatchObject({
      changes: {
        "file:///test.ml": [
          {
            range: {
              start: {
                line: 0,
                character: 4,
              },
              end: {
                line: 0,
                character: 7,
              },
            },
            newText: "new_num",
          },
          {
            range: {
              start: {
                line: 1,
                character: 10,
              },
              end: {
                line: 1,
                character: 13,
              },
            },
            newText: "new_num",
          },
        ],
      },
    });
  });

  it("rename value in a file with documentChanges capability", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      capabilities: {
        workspace: { workspaceEdit: { documentChanges: true } },
      },
    });

    await openDocument(outdent`
      let num = 42
      let num = num + 13
      let num2 = num
    `);

    let result = await query(Types.Position.create(0, 4));

    expect(result).toMatchObject({
      documentChanges: [
        {
          textDocument: {
            version: 0,
            uri: "file:///test.ml",
          },
          edits: [
            {
              range: {
                start: {
                  line: 0,
                  character: 4,
                },
                end: {
                  line: 0,
                  character: 7,
                },
              },
              newText: "new_num",
            },
            {
              range: {
                start: {
                  line: 1,
                  character: 10,
                },
                end: {
                  line: 1,
                  character: 13,
                },
              },
              newText: "new_num",
            },
          ],
        },
      ],
    });
  });

  it("rename a var used as a named argument", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      capabilities: {
        workspace: { workspaceEdit: { documentChanges: false } },
      },
    });

    await openDocument(outdent`
let foo x = x

let bar ~foo = foo ()

let () = bar ~foo
    `);

    let result = await query(Types.Position.create(0, 4), "ident");

    expect(result).toMatchInlineSnapshot(`
      Object {
        "changes": Object {
          "file:///test.ml": Array [
            Object {
              "newText": "ident",
              "range": Object {
                "end": Object {
                  "character": 7,
                  "line": 0,
                },
                "start": Object {
                  "character": 4,
                  "line": 0,
                },
              },
            },
            Object {
              "newText": ":ident",
              "range": Object {
                "end": Object {
                  "character": 17,
                  "line": 4,
                },
                "start": Object {
                  "character": 17,
                  "line": 4,
                },
              },
            },
          ],
        },
      }
    `);
  });

  it("rename a var used as a named argument", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      capabilities: {
        workspace: { workspaceEdit: { documentChanges: false } },
      },
    });

    await openDocument(outdent`
let foo = Some ()

let bar ?foo () = foo

;;
ignore (bar ?foo ())
    `);

    let result = await query(Types.Position.create(0, 4), "sunit");

    expect(result).toMatchInlineSnapshot(`
      Object {
        "changes": Object {
          "file:///test.ml": Array [
            Object {
              "newText": "sunit",
              "range": Object {
                "end": Object {
                  "character": 7,
                  "line": 0,
                },
                "start": Object {
                  "character": 4,
                  "line": 0,
                },
              },
            },
            Object {
              "newText": ":sunit",
              "range": Object {
                "end": Object {
                  "character": 16,
                  "line": 5,
                },
                "start": Object {
                  "character": 16,
                  "line": 5,
                },
              },
            },
          ],
        },
      }
    `);
  });
});
