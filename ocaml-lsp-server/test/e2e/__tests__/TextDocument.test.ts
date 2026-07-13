import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";
import * as LanguageServer from "./../src/LanguageServer";

describe("TextDocument", () => {
  let languageServer: LanguageServer.LanguageServer;

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
  });

  describe("didOpen", () => {
  });

  describe("didChange", () => {
    it("updates text document", async () => {
      languageServer = await LanguageServer.startAndInitialize();
      languageServer.sendNotification(
        Protocol.DidOpenTextDocumentNotification.type,
        {
          textDocument: Types.TextDocumentItem.create(
            "file:///test-document.txt",
            "ocaml",
            0,
            "Hello, World!",
          ),
        },
      );

      languageServer.sendNotification(
        Protocol.DidChangeTextDocumentNotification.type,
        {
          textDocument: Types.VersionedTextDocumentIdentifier.create(
            "file:///test-document.txt",
            1,
          ),
          contentChanges: [{ text: "Hello again!" }],
        },
      );

      const result: string = await languageServer.sendRequest(
        "debug/textDocument/get",
        {
          textDocument: Types.TextDocumentIdentifier.create(
            "file:///test-document.txt",
          ),
          position: Types.Position.create(0, 0),
        },
      );

      expect(result).toEqual("Hello again!");
    });
  });
});
