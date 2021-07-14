import outdent from "outdent";
import * as LanguageServer from "./../src/LanguageServer";

import * as Types from "vscode-languageserver-types";
import { Position } from "vscode-languageserver-types";

const describe_opt = LanguageServer.ocamlVersionGEq("4.08.0")
  ? describe
  : xdescribe;

describe_opt("textDocument/completion", () => {
  let languageServer = null;

  async function openDocument(source) {
    return languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test.ml",
        "ocaml",
        0,
        source,
      ),
    });
  }

  async function queryCompletion(position) {
    let result = await languageServer.sendRequest("textDocument/completion", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
      position,
    });
    return result.items.map((item) => {
      return {
        label: item.label,
        textEdit: item.textEdit,
      };
    });
  }

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
    languageServer = null;
  });

  it("can start completion at arbitrary position (before the dot)", async () => {
    await openDocument(outdent`
      Strin.func
    `);

    let items = await queryCompletion(Types.Position.create(0, 5));
    expect(items).toMatchObject([
      { label: "String" },
      { label: "StringLabels" },
    ]);
  });

  it("can start completion at arbitrary position", async () => {
    await openDocument(outdent`
      StringLabels
    `);

    let items = await queryCompletion(Types.Position.create(0, 6));
    expect(items).toMatchObject([
      { label: "String" },
      { label: "StringLabels" },
    ]);
  });

  it("can start completion at arbitrary position 2", async () => {
    await openDocument(outdent`
      StringLabels
    `);

    let items = await queryCompletion(Types.Position.create(0, 7));
    expect(items).toMatchObject([{ label: "StringLabels" }]);
  });

  it("completes identifier at top level", async () => {
    await openDocument(outdent`
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
    await openDocument(outdent`
      module Test = struct
        let somenum = 42
        let somestring = "hello"
      end

      let x = Test.
    `);

    let items = await queryCompletion(Types.Position.create(5, 13));

    expect(items).toMatchInlineSnapshot(`
      Array [
        Object {
          "label": "somenum",
          "textEdit": Object {
            "newText": "somenum",
            "range": Object {
              "end": Object {
                "character": 13,
                "line": 5,
              },
              "start": Object {
                "character": 13,
                "line": 5,
              },
            },
          },
        },
        Object {
          "label": "somestring",
          "textEdit": Object {
            "newText": "somestring",
            "range": Object {
              "end": Object {
                "character": 13,
                "line": 5,
              },
              "start": Object {
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
    await openDocument(outdent`
      let (>>|) = (+)
      let y = 1 >
    `);

    let items = await queryCompletion(Types.Position.create(1, 11));
    expect(items).toMatchInlineSnapshot(`
      Array [
        Object {
          "label": ">>|",
          "textEdit": Object {
            "newText": ">>|",
            "range": Object {
              "end": Object {
                "character": 11,
                "line": 1,
              },
              "start": Object {
                "character": 10,
                "line": 1,
              },
            },
          },
        },
        Object {
          "label": ">",
          "textEdit": Object {
            "newText": ">",
            "range": Object {
              "end": Object {
                "character": 11,
                "line": 1,
              },
              "start": Object {
                "character": 10,
                "line": 1,
              },
            },
          },
        },
        Object {
          "label": ">=",
          "textEdit": Object {
            "newText": ">=",
            "range": Object {
              "end": Object {
                "character": 11,
                "line": 1,
              },
              "start": Object {
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
    await openDocument(outdent`
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
    await openDocument(outdent`
      let f = L
    `);

    let items = await queryCompletion(Types.Position.create(0, 9));
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
    await openDocument(outdent`
      let somenum = 42
      let somestring = "hello"

      let plus_42 (x:int) (y:int) =
        somenum +    `);

    let items = await queryCompletion(Types.Position.create(4, 12));
    let items_top5 = items.slice(0, 5);
    expect(items_top5).toMatchInlineSnapshot(`
      Array [
        Object {
          "label": "somenum",
          "textEdit": Object {
            "newText": "somenum",
            "range": Object {
              "end": Object {
                "character": 12,
                "line": 4,
              },
              "start": Object {
                "character": 12,
                "line": 4,
              },
            },
          },
        },
        Object {
          "label": "x",
          "textEdit": Object {
            "newText": "x",
            "range": Object {
              "end": Object {
                "character": 12,
                "line": 4,
              },
              "start": Object {
                "character": 12,
                "line": 4,
              },
            },
          },
        },
        Object {
          "label": "y",
          "textEdit": Object {
            "newText": "y",
            "range": Object {
              "end": Object {
                "character": 12,
                "line": 4,
              },
              "start": Object {
                "character": 12,
                "line": 4,
              },
            },
          },
        },
        Object {
          "label": "max_int",
          "textEdit": Object {
            "newText": "max_int",
            "range": Object {
              "end": Object {
                "character": 12,
                "line": 4,
              },
              "start": Object {
                "character": 12,
                "line": 4,
              },
            },
          },
        },
        Object {
          "label": "min_int",
          "textEdit": Object {
            "newText": "min_int",
            "range": Object {
              "end": Object {
                "character": 12,
                "line": 4,
              },
              "start": Object {
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
    await openDocument("let f = ListLabels.map ~");

    let items = await queryCompletion(Types.Position.create(0, 24));
    let items_top5 = items.slice(0, 10);
    expect(items_top5).toMatchInlineSnapshot(`
      Array [
        Object {
          "label": "~+",
          "textEdit": Object {
            "newText": "~+",
            "range": Object {
              "end": Object {
                "character": 24,
                "line": 0,
              },
              "start": Object {
                "character": 23,
                "line": 0,
              },
            },
          },
        },
        Object {
          "label": "~+.",
          "textEdit": Object {
            "newText": "~+.",
            "range": Object {
              "end": Object {
                "character": 24,
                "line": 0,
              },
              "start": Object {
                "character": 23,
                "line": 0,
              },
            },
          },
        },
        Object {
          "label": "~-",
          "textEdit": Object {
            "newText": "~-",
            "range": Object {
              "end": Object {
                "character": 24,
                "line": 0,
              },
              "start": Object {
                "character": 23,
                "line": 0,
              },
            },
          },
        },
        Object {
          "label": "~-.",
          "textEdit": Object {
            "newText": "~-.",
            "range": Object {
              "end": Object {
                "character": 24,
                "line": 0,
              },
              "start": Object {
                "character": 23,
                "line": 0,
              },
            },
          },
        },
        Object {
          "label": "~f",
          "textEdit": Object {
            "newText": "~f",
            "range": Object {
              "end": Object {
                "character": 24,
                "line": 0,
              },
              "start": Object {
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
    await openDocument(outdent`
      type r = {
        x: int;
        y: string
      }

      let _ =
    `);

    let items: Array<any> = await queryCompletion(Types.Position.create(5, 8));
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
      Array [
        Object {
          "label": "\`String",
          "textEdit": Object {
            "newText": "\`String",
            "range": Object {
              "end": Object {
                "character": 15,
                "line": 2,
              },
              "start": Object {
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
      Array [
        Object {
          "label": "\`Int",
          "textEdit": Object {
            "newText": "\`Int",
            "range": Object {
              "end": Object {
                "character": 14,
                "line": 2,
              },
              "start": Object {
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
      Array [
        Object {
          "label": "\`Int",
          "textEdit": Object {
            "newText": "\`Int",
            "range": Object {
              "end": Object {
                "character": 15,
                "line": 2,
              },
              "start": Object {
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
    await openDocument(outdent`
let u : int = _
`);

    let items: Types.CompletionItem[] = await queryCompletion(
      Types.Position.create(0, 15),
    );

    items = items.filter(
      (completionItem) => !completionItem.label.startsWith("__"),
    );

    expect(items).toMatchInlineSnapshot(`
      Array [
        Object {
          "label": "0",
          "textEdit": Object {
            "newText": "0",
            "range": Object {
              "end": Object {
                "character": 15,
                "line": 0,
              },
              "start": Object {
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
