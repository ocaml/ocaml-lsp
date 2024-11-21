import outdent from "outdent";
import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";
import * as LanguageServer from "../src/LanguageServer";

describe("textDocument/rename", () => {
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

  async function query(position: Types.Position, newNameOpt?: string) {
    const newName = newNameOpt ? newNameOpt : "new_num";
    return await languageServer.sendRequest(Protocol.RenameRequest.type, {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
      position,
      newName,
    });
  }

  async function query_prepare(position: Types.Position) {
    return await languageServer.sendRequest(
      Protocol.PrepareRenameRequest.type,
      {
        textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
        position,
      },
    );
  }

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
  });

  it("can reject invalid rename request", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      capabilities: {
        workspace: { workspaceEdit: { documentChanges: false } },
      },
    });

    openDocument(outdent`
      let num = 42
      let num = num + 13
      let num2 = num
    `);

    const result = await query_prepare(Types.Position.create(0, 1));
    expect(result).toBeNull();
  });

  it("allows valid rename request", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      capabilities: {
        workspace: { workspaceEdit: { documentChanges: false } },
      },
    });

    openDocument(outdent`
      let num = 42
      let num = num + 13
      let num2 = num
    `);

    const result = await query_prepare(Types.Position.create(0, 4));
    expect(result).toMatchInlineSnapshot(`
{
  "end": {
    "character": 7,
    "line": 0,
  },
  "start": {
    "character": 4,
    "line": 0,
  },
}
`);
  });

  it("rename value in a file without documentChanges capability", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      capabilities: {
        workspace: { workspaceEdit: { documentChanges: false } },
      },
    });

    openDocument(outdent`
      let num = 42
      let num = num + 13
      let num2 = num
    `);

    const result = await query(Types.Position.create(0, 4));

    expect(result).toMatchInlineSnapshot(`
{
  "changes": {
    "file:///test.ml": [
      {
        "newText": "new_num",
        "range": {
          "end": {
            "character": 13,
            "line": 1,
          },
          "start": {
            "character": 10,
            "line": 1,
          },
        },
      },
      {
        "newText": "new_num",
        "range": {
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
    ],
  },
}
`);
  });

  it("rename value in a file with documentChanges capability", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      capabilities: {
        workspace: { workspaceEdit: { documentChanges: true } },
      },
    });

    openDocument(outdent`
      let num = 42
      let num = num + 13
      let num2 = num
    `);

    const result = await query(Types.Position.create(0, 4));

    expect(result).toMatchInlineSnapshot(`
{
  "documentChanges": [
    {
      "edits": [
        {
          "newText": "new_num",
          "range": {
            "end": {
              "character": 13,
              "line": 1,
            },
            "start": {
              "character": 10,
              "line": 1,
            },
          },
        },
        {
          "newText": "new_num",
          "range": {
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
      ],
      "textDocument": {
        "uri": "file:///test.ml",
        "version": 0,
      },
    },
  ],
}
`);
  });

  it("rename a var used as a named argument", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      capabilities: {
        workspace: { workspaceEdit: { documentChanges: false } },
      },
    });

    openDocument(outdent`
let foo x = x

let bar ~foo = foo ()

let () = bar ~foo
    `);

    const result = await query(Types.Position.create(0, 4), "ident");

    expect(result).toMatchInlineSnapshot(`
{
  "changes": {
    "file:///test.ml": [
      {
        "newText": ":ident",
        "range": {
          "end": {
            "character": 17,
            "line": 4,
          },
          "start": {
            "character": 17,
            "line": 4,
          },
        },
      },
      {
        "newText": "ident",
        "range": {
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

    openDocument(outdent`
let foo = Some ()

let bar ?foo () = foo

;;
ignore (bar ?foo ())
    `);

    const result = await query(Types.Position.create(0, 4), "sunit");

    expect(result).toMatchInlineSnapshot(`
{
  "changes": {
    "file:///test.ml": [
      {
        "newText": ":sunit",
        "range": {
          "end": {
            "character": 16,
            "line": 5,
          },
          "start": {
            "character": 16,
            "line": 5,
          },
        },
      },
      {
        "newText": "sunit",
        "range": {
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
    ],
  },
}
`);
  });
});
