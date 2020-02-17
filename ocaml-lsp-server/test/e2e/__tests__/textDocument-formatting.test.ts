import outdent from "outdent";
import * as LanguageServer from "../src/LanguageServer";

import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";

describe("textDocument/formatting", () => {
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

  async function query() {
    return await languageServer.sendRequest("textDocument/formatting", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
      options: Types.FormattingOptions.create(2, true),
    });
  }

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
    languageServer = null;
  });

  it("is a dummy test", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      workspace: { workspaceEdit: { documentChanges: false } },
    });

    console.log(process.cwd());

    await openDocument(outdent`
      let rec gcd a b =
        begin match a, b with
          | 0, n | n, 0 -> n
          | _, _ ->
            gcd a (b mod a)
        end
    `);

    let result = await query();
    console.log(result);
  });
});
