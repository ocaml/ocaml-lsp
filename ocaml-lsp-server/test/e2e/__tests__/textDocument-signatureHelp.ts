import outdent from "outdent";
import * as LanguageServer from "./../src/LanguageServer";

import * as Types from "vscode-languageserver-types";

const describe_opt = LanguageServer.ocamlVersionGEq("4.08.0")
  ? describe
  : xdescribe;

describe_opt("textDocument/completion", () => {
  let languageServer = null;

  async function openDocument(source) {
    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test.ml",
        "ocaml",
        0,
        source,
      ),
    });
  }

  async function querySignatureHelp(position) {
    return await languageServer.sendRequest("textDocument/signatureHelp", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
      position,
    });
  }

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
    languageServer = null;
  });

  it("can provide signature help after a function-type value", async () => {
    openDocument(outdent`
      let _ = ListLabels.map
    `);

    let items = await querySignatureHelp(Types.Position.create(0, 22));
    expect(items).toMatchObject({
      signatures: [
        {
          label: "ListLabels.map : f:('a -> 'b) -> 'a list -> 'b list",
          parameters: [
            {
              label: [17, 29],
            },
            {
              label: [33, 40],
            },
          ],
        },
      ],
      activeSignature: 0,
      activeParameter: 1,
    });
  });

  it("can provide signature help for an operator", async () => {
    openDocument(outdent`
      let _ = 1 + 2
    `);

    let items = await querySignatureHelp(Types.Position.create(0, 13));
    expect(items).toMatchObject({
      signatures: [
        {
          label: "(+) : int -> int -> int",
          parameters: [
            {
              label: [6, 9],
            },
            {
              label: [13, 16],
            },
          ],
        },
      ],
      activeSignature: 0,
      activeParameter: 1,
    });
  });

  it("can provide signature help for an anonymous function", async () => {
    openDocument(outdent`
      let _ = (fun x -> x + 1)
    `);

    let items = await querySignatureHelp(Types.Position.create(0, 26));
    expect(items).toMatchObject({
      signatures: [
        {
          label: "_ : int -> int",
          parameters: [
            {
              label: [4, 7],
            },
          ],
        },
      ],
      activeSignature: 0,
      activeParameter: 0,
    });
  });

  it("can make the non-labelled parameter active", async () => {
    openDocument(outdent`
      let _ = ListLabels.map []
    `);

    let items = await querySignatureHelp(Types.Position.create(0, 25));
    expect(items).toMatchObject({
      signatures: [
        {
          label: "ListLabels.map : f:('a -> 'b) -> 'a list -> 'b list",
          parameters: [
            {
              label: [17, 29],
            },
            {
              label: [33, 40],
            },
          ],
        },
      ],
      activeSignature: 0,
      activeParameter: 1,
    });
  });

  it("can make the labelled parameter active", async () => {
    openDocument(outdent`
      let _ = ListLabels.map ~f:Int.abs
    `);

    let items = await querySignatureHelp(Types.Position.create(0, 33));
    expect(items).toMatchObject({
      signatures: [
        {
          label: "ListLabels.map : f:(int -> int) -> int list -> int list",
          parameters: [
            {
              label: [17, 31],
            },
            {
              label: [35, 43],
            },
          ],
        },
      ],
      activeSignature: 0,
      activeParameter: 0,
    });
  });

  it("can make a labelled parameter active by prefix", async () => {
    openDocument(outdent`
      let _ = ListLabels.mem ~se
    `);

    let items = await querySignatureHelp(Types.Position.create(0, 26));
    expect(items).toMatchObject({
      signatures: [
        {
          label: "ListLabels.mem : 'a -> set:'a list -> bool",
          parameters: [
            {
              label: [17, 19],
            },
            {
              label: [23, 34],
            },
          ],
        },
      ],
      activeSignature: 0,
      activeParameter: 1,
    });
  });
});
