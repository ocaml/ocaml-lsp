import outdent from "outdent";
import * as LanguageServer from "./../src/LanguageServer";

import * as Types from "vscode-languageserver-types";

xdescribe("textDocument/completion", () => {
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
    openDocument(outdent`
      let somenum = 42
      let somestring = "hello"

      let plus_42 (x:int) (y:int) =
        somenum +
    `);

    let items = await queryCompletion(Types.Position.create(4, 12));
    let items_top5 = items.slice(0, 5);
    expect(items_top5).toMatchObject([
      { label: "somenum" },
      { label: "x" },
      { label: "y" },
      { label: "max_int" },
      { label: "min_int" },
    ]);
  });

  it("completes with invalid prefix", async () => {
    openDocument(outdent`
      let f = Li.ma
    `);

    let items = await queryCompletion(Types.Position.create(0, 13));
    expect(items).toMatchObject([
      {
        label: "ListLabels.map",
        textEdit: {
          range: {
            start: { line: 0, character: 8 },
            end: { line: 0, character: 13 },
          },
          newText: "ListLabels.map",
        },
      },
      {
        label: "ListLabels.map2",
        textEdit: {
          range: {
            start: { line: 0, character: 8 },
            end: { line: 0, character: 13 },
          },
          newText: "ListLabels.map2",
        },
      },
      {
        label: "ListLabels.mapi",
        textEdit: {
          range: {
            start: { line: 0, character: 8 },
            end: { line: 0, character: 13 },
          },
          newText: "ListLabels.mapi",
        },
      },
      {
        label: "List.map",
        textEdit: {
          range: {
            start: { line: 0, character: 8 },
            end: { line: 0, character: 13 },
          },
          newText: "List.map",
        },
      },
      {
        label: "List.map2",
        textEdit: {
          range: {
            start: { line: 0, character: 8 },
            end: { line: 0, character: 13 },
          },
          newText: "List.map2",
        },
      },
      {
        label: "List.mapi",
        textEdit: {
          range: {
            start: { line: 0, character: 8 },
            end: { line: 0, character: 13 },
          },
          newText: "List.mapi",
        },
      },
    ]);
  });

  it("completes with invalid prefix is buggy, it gives suggestions for LL instead of L", async () => {
    openDocument(outdent`
      let f = L.
    `);

    let items = await queryCompletion(Types.Position.create(0, 10));
    let items_top5 = items.slice(0, 5);
    expect(items_top5).toMatchObject([
      {
        label: "ListLabels.((::))",
        textEdit: {
          range: {
            start: { line: 0, character: 8 },
            end: { line: 0, character: 10 },
          },
          newText: "ListLabels.((::))",
        },
      },
      {
        label: "ListLabels.[]",
        textEdit: {
          range: {
            start: { line: 0, character: 8 },
            end: { line: 0, character: 10 },
          },
          newText: "ListLabels.[]",
        },
      },
      {
        label: "ListLabels.t",
        textEdit: {
          range: {
            start: { line: 0, character: 8 },
            end: { line: 0, character: 10 },
          },
          newText: "ListLabels.t",
        },
      },
      {
        label: "ListLabels.append",
        textEdit: {
          range: {
            start: { line: 0, character: 8 },
            end: { line: 0, character: 10 },
          },
          newText: "ListLabels.append",
        },
      },
      {
        label: "ListLabels.assoc",
        textEdit: {
          range: {
            start: { line: 0, character: 8 },
            end: { line: 0, character: 10 },
          },
          newText: "ListLabels.assoc",
        },
      },
    ]);
  });

  it("completes with invalid prefix is buggy", async () => {
    openDocument(outdent`
      let f = LL.
    `);

    let items = await queryCompletion(Types.Position.create(0, 11));
    expect(items).toMatchObject([]);
  });

  it("completes labels", async () => {
    openDocument(outdent`
      let f = ListLabels.map
    `);

    let items = await queryCompletion(Types.Position.create(0, 23));
    let items_top5 = items.slice(0, 5);
    expect(items_top5).toMatchObject([
      { label: "~f" },
      { label: "::" },
      { label: "[]" },
      { label: "!" },
      { label: "exit" },
    ]);
  });
});
