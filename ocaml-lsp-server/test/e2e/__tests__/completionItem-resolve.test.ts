import outdent from "outdent";
import * as LanguageServer from "./../src/LanguageServer";

import * as Types from "vscode-languageserver-types";

xdescribe("textDocument/completion", () => {
  let languageServer: LanguageServer.LanguageServer = null;

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

  async function queryCompletionItemResolve(
    label: string,
    position: Types.Position,
  ) {
    let response = await languageServer.sendRequest("completionItem/resolve", {
      label: label,
      data: {
        textDocument: {
          uri: "file:///test.ml",
        },
        position: position,
      },
    });
  }

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
    languageServer = null;
  });

  it("can get documentation for the end of document", async () => {
    openDocument(outdent`
      List.ma
    `);

    let response = await queryCompletionItemResolve(
      "map2",
      Types.Position.create(0, 5),
    );

    expect(response).toMatchObject({
      documentation: {
        kind: "markdown",
        value:
          "`List.map2 f [a1; ...; an] [b1; ...; bn]` is\n`[f a1 b1; ...; f an bn]`.\nRaise `Invalid_argument` if the two lists are determined\nto have different lengths. Not tail-recursive.",
      },
    });
  });

  it("can get documentation at arbitrary position", async () => {
    openDocument(outdent`
      List.fld((=) 0) [1; 2; 3]
    `);

    let response = await queryCompletionItemResolve(
      "find_all",
      Types.Position.create(0, 5),
    );

    expect(response).toMatchObject({
      documentation: {
        kind: "markdown",
        value: "`find_all` is another name for `List.filter`.",
      },
    });
  });

  it("can get documentation at arbitrary position (before dot)", async () => {
    openDocument(outdent`
    Stdlib.LargeFil.in_channel_length
    `);

    let response = await queryCompletionItemResolve(
      "find_all",
      Types.Position.create(0, 15),
    );

    expect(response).toMatchObject({
      documentation: {
        kind: "markdown",
        value:
          "Operations on large files.\nThis sub-module provides 64-bit variants of the channel functions\nthat manipulate file positions and file sizes. By representing\npositions and sizes by 64-bit integers \\(type `int64`\\) instead of\nregular integers \\(type `int`\\), these alternate functions allow\noperating on files whose sizes are greater than `max_int`.",
      },
    });
  });
});
