import outdent from "outdent";
import * as LanguageServer from "./../src/LanguageServer";
import * as Protocol from "vscode-languageserver-protocol";

import * as Types from "vscode-languageserver-types";
import { Position } from "vscode-languageserver-types";

const describe_opt = LanguageServer.ocamlVersionGEq("4.08.0")
  ? describe
  : xdescribe;

describe_opt("textDocument/completion", () => {
  let languageServer: LanguageServer.LanguageServer;

  function openDocument(source: string) {
    return languageServer.sendNotification(
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

  async function queryCompletion(position: Types.Position) {
    let result =
      (await languageServer.sendRequest(Protocol.CompletionRequest.type, {
        textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
        position,
      })) ?? [];

    if ("items" in result) {
      return result.items.map((item) => {
        return {
          label: item.label,
          textEdit: item.textEdit,
        };
      });
    } else {
      result.map((item) => {
        return {
          label: item.label,
          textEdit: item.textEdit,
        };
      });
    }
  }

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
  });

  it("can start completion at arbitrary position (before the dot)", async () => {
    openDocument(outdent`
      Strin.func
    `);

    let items = await queryCompletion(Types.Position.create(0, 5));
    expect(items).toMatchObject([
      { label: "String" },
      { label: "StringLabels" },
    ]);
  });

  it("can start completion at arbitrary position", async () => {
    openDocument(outdent`
      StringLabels
    `);

    let items = await queryCompletion(Types.Position.create(0, 6));
    expect(items).toMatchObject([
      { label: "String" },
      { label: "StringLabels" },
    ]);
  });

  it("can start completion at arbitrary position 2", async () => {
    openDocument(outdent`
      StringLabels
    `);

    let items = await queryCompletion(Types.Position.create(0, 7));
    expect(items).toMatchObject([{ label: "StringLabels" }]);
  });

  it("can complete symbol passed as a named argument", async () => {
    openDocument(outdent`
let g ~f = f 0 in
g ~f:ig
    `);

    let items = await queryCompletion(Types.Position.create(1, 7));
    expect(items).toMatchInlineSnapshot(`
      [
        {
          "label": "ignore",
          "textEdit": {
            "newText": "ignore",
            "range": {
              "end": {
                "character": 7,
                "line": 1,
              },
              "start": {
                "character": 5,
                "line": 1,
              },
            },
          },
        },
      ]
    `);
  });

  it("can complete symbol passed as a named argument - 2", async () => {
    openDocument(outdent`
module M = struct let igfoo _x = () end
let g ~f = f 0 in
g ~f:M.ig
    `);

    let items = await queryCompletion(Types.Position.create(2, 9));
    expect(items).toMatchInlineSnapshot(`
      [
        {
          "label": "igfoo",
          "textEdit": {
            "newText": "igfoo",
            "range": {
              "end": {
                "character": 9,
                "line": 2,
              },
              "start": {
                "character": 7,
                "line": 2,
              },
            },
          },
        },
      ]
    `);
  });

  it("can complete symbol passed as an optional argument", async () => {
    openDocument(outdent`
let g ?f = f in
g ?f:ig
    `);

    let items = await queryCompletion(Types.Position.create(1, 7));
    expect(items).toMatchInlineSnapshot(`
      [
        {
          "label": "ignore",
          "textEdit": {
            "newText": "ignore",
            "range": {
              "end": {
                "character": 7,
                "line": 1,
              },
              "start": {
                "character": 5,
                "line": 1,
              },
            },
          },
        },
      ]
    `);
  });

  it("can complete symbol passed as a optional argument - 2", async () => {
    openDocument(outdent`
module M = struct let igfoo _x = () end
let g ?f = f in
g ?f:M.ig
    `);

    let items = await queryCompletion(Types.Position.create(2, 9));
    expect(items).toMatchInlineSnapshot(`
      [
        {
          "label": "igfoo",
          "textEdit": {
            "newText": "igfoo",
            "range": {
              "end": {
                "character": 9,
                "line": 2,
              },
              "start": {
                "character": 7,
                "line": 2,
              },
            },
          },
        },
      ]
    `);
  });

  it("completes identifier at top level", async () => {
    openDocument(outdent`
      let somenum = 42
      let somestring = "hello"

      let () =
        some
    `);

    let items = await queryCompletion(Types.Position.create(4, 6));
    expect(items).toMatchObject([
      { label: "somenum" },
      { label: "somestring" },
    ]);
  });

  it("completes identifier after completion-triggering character", async () => {
    openDocument(outdent`
      module Test = struct
        let somenum = 42
        let somestring = "hello"
      end

      let x = Test.
    `);

    let items = await queryCompletion(Types.Position.create(5, 13));

    expect(items).toMatchInlineSnapshot(`
      [
        {
          "label": "somenum",
          "textEdit": {
            "newText": "somenum",
            "range": {
              "end": {
                "character": 13,
                "line": 5,
              },
              "start": {
                "character": 13,
                "line": 5,
              },
            },
          },
        },
        {
          "label": "somestring",
          "textEdit": {
            "newText": "somestring",
            "range": {
              "end": {
                "character": 13,
                "line": 5,
              },
              "start": {
                "character": 13,
                "line": 5,
              },
            },
          },
        },
      ]
    `);
  });

  it("completes infix operators", async () => {
    openDocument(outdent`
      let (>>|) = (+)
      let y = 1 >
    `);

    let items = await queryCompletion(Types.Position.create(1, 11));
    expect(items).toMatchInlineSnapshot(`
      [
        {
          "label": ">>|",
          "textEdit": {
            "newText": ">>|",
            "range": {
              "end": {
                "character": 11,
                "line": 1,
              },
              "start": {
                "character": 10,
                "line": 1,
              },
            },
          },
        },
        {
          "label": ">",
          "textEdit": {
            "newText": ">",
            "range": {
              "end": {
                "character": 11,
                "line": 1,
              },
              "start": {
                "character": 10,
                "line": 1,
              },
            },
          },
        },
        {
          "label": ">=",
          "textEdit": {
            "newText": ">=",
            "range": {
              "end": {
                "character": 11,
                "line": 1,
              },
              "start": {
                "character": 10,
                "line": 1,
              },
            },
          },
        },
      ]
    `);
  });

  it("completes from a module", async () => {
    openDocument(outdent`
      let f = List.m
    `);

    let items = await queryCompletion(Types.Position.create(0, 14));
    expect(items).toMatchObject([
      { label: "map" },
      { label: "map2" },
      { label: "mapi" },
      { label: "mem" },
      { label: "mem_assoc" },
      { label: "mem_assq" },
      { label: "memq" },
      { label: "merge" },
    ]);
  });

  it("completes a module name", async () => {
    openDocument(outdent`
      let f = L
    `);

    let items = (await queryCompletion(Types.Position.create(0, 9))) ?? [];
    let items_top5 = items.slice(0, 5);
    expect(items_top5).toMatchObject([
      { label: "LargeFile" },
      { label: "Lazy" },
      { label: "Lexing" },
      { label: "List" },
      { label: "ListLabels" },
    ]);
  });

  it("completes without prefix", async () => {
    openDocument(outdent`
      let somenum = 42
      let somestring = "hello"

      let plus_42 (x:int) (y:int) =
        somenum +    `);

    let items = (await queryCompletion(Types.Position.create(4, 12))) ?? [];
    let items_top5 = items.slice(0, 5);
    expect(items_top5).toMatchInlineSnapshot(`
      [
        {
          "label": "somenum",
          "textEdit": {
            "newText": "somenum",
            "range": {
              "end": {
                "character": 12,
                "line": 4,
              },
              "start": {
                "character": 12,
                "line": 4,
              },
            },
          },
        },
        {
          "label": "x",
          "textEdit": {
            "newText": "x",
            "range": {
              "end": {
                "character": 12,
                "line": 4,
              },
              "start": {
                "character": 12,
                "line": 4,
              },
            },
          },
        },
        {
          "label": "y",
          "textEdit": {
            "newText": "y",
            "range": {
              "end": {
                "character": 12,
                "line": 4,
              },
              "start": {
                "character": 12,
                "line": 4,
              },
            },
          },
        },
        {
          "label": "max_int",
          "textEdit": {
            "newText": "max_int",
            "range": {
              "end": {
                "character": 12,
                "line": 4,
              },
              "start": {
                "character": 12,
                "line": 4,
              },
            },
          },
        },
        {
          "label": "min_int",
          "textEdit": {
            "newText": "min_int",
            "range": {
              "end": {
                "character": 12,
                "line": 4,
              },
              "start": {
                "character": 12,
                "line": 4,
              },
            },
          },
        },
      ]
    `);
  });

  it("completes labels", async () => {
    openDocument("let f = ListLabels.map ~");

    let items = (await queryCompletion(Types.Position.create(0, 24))) ?? [];
    let items_top5 = items.slice(0, 10);
    expect(items_top5).toMatchInlineSnapshot(`
      [
        {
          "label": "~+",
          "textEdit": {
            "newText": "~+",
            "range": {
              "end": {
                "character": 24,
                "line": 0,
              },
              "start": {
                "character": 23,
                "line": 0,
              },
            },
          },
        },
        {
          "label": "~+.",
          "textEdit": {
            "newText": "~+.",
            "range": {
              "end": {
                "character": 24,
                "line": 0,
              },
              "start": {
                "character": 23,
                "line": 0,
              },
            },
          },
        },
        {
          "label": "~-",
          "textEdit": {
            "newText": "~-",
            "range": {
              "end": {
                "character": 24,
                "line": 0,
              },
              "start": {
                "character": 23,
                "line": 0,
              },
            },
          },
        },
        {
          "label": "~-.",
          "textEdit": {
            "newText": "~-.",
            "range": {
              "end": {
                "character": 24,
                "line": 0,
              },
              "start": {
                "character": 23,
                "line": 0,
              },
            },
          },
        },
        {
          "label": "~f",
          "textEdit": {
            "newText": "~f",
            "range": {
              "end": {
                "character": 24,
                "line": 0,
              },
              "start": {
                "character": 23,
                "line": 0,
              },
            },
          },
        },
      ]
    `);
  });

  it("completion doesn't autocomplete record fields", async () => {
    openDocument(outdent`
      type r = {
        x: int;
        y: string
      }

      let _ =
    `);

    let items = (await queryCompletion(Types.Position.create(5, 8))) ?? [];
    expect(
      items.filter((compl) => compl.label === "x" || compl.label === "y"),
    ).toHaveLength(0);
  });

  it("works for polymorphic variants - function application context - 1", async () => {
    openDocument(outdent`
let f (_a: [\`String | \`Int of int]) = ()

let u = f \`Str
    `);

    let items = await queryCompletion(Position.create(2, 15));

    expect(items).toMatchInlineSnapshot(`
      [
        {
          "label": "\`String",
          "textEdit": {
            "newText": "\`String",
            "range": {
              "end": {
                "character": 15,
                "line": 2,
              },
              "start": {
                "character": 11,
                "line": 2,
              },
            },
          },
        },
      ]
    `);
  });

  it("works for polymorphic variants - function application context - 2", async () => {
    openDocument(outdent`
let f (_a: [\`String | \`Int of int]) = ()

let u = f \`In
    `);

    let items = await queryCompletion(Position.create(2, 14));

    expect(items).toMatchInlineSnapshot(`
      [
        {
          "label": "\`Int",
          "textEdit": {
            "newText": "\`Int",
            "range": {
              "end": {
                "character": 14,
                "line": 2,
              },
              "start": {
                "character": 11,
                "line": 2,
              },
            },
          },
        },
      ]
    `);
  });

  it("works for polymorphic variants", async () => {
    openDocument(outdent`
type t = [ \`Int | \`String ]

let x : t = \`I
    `);

    let items = await queryCompletion(Position.create(2, 15));

    expect(items).toMatchInlineSnapshot(`
      [
        {
          "label": "\`Int",
          "textEdit": {
            "newText": "\`Int",
            "range": {
              "end": {
                "character": 15,
                "line": 2,
              },
              "start": {
                "character": 13,
                "line": 2,
              },
            },
          },
        },
      ]
    `);
  });

  it("completion for holes", async () => {
    openDocument(outdent`
let u : int = _
`);

    let items = (await queryCompletion(Types.Position.create(0, 15))) ?? [];

    items = items.filter(
      (completionItem) => !completionItem.label.startsWith("__"),
    );

    expect(items).toMatchInlineSnapshot(`
      [
        {
          "label": "0",
          "textEdit": {
            "newText": "0",
            "range": {
              "end": {
                "character": 15,
                "line": 0,
              },
              "start": {
                "character": 14,
                "line": 0,
              },
            },
          },
        },
      ]
    `);
  });
});
