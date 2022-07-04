import outdent from "outdent";
import * as rpc from "vscode-jsonrpc/node";
import * as LanguageServer from "../src/LanguageServer";
import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";
import { URI } from "vscode-uri";

describe("uri", () => {
  let languageServer: rpc.MessageConnection;
  const uri = URI.file("src/néw/Mödel + Other Thîngß/test.ml").toString();

  function openDocument(source: string) {
    languageServer.sendNotification(
      Protocol.DidOpenTextDocumentNotification.type,
      {
        textDocument: Types.TextDocumentItem.create(uri, "ocaml", 0, source),
      },
    );
  }

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
  });

  it("handles uri with special characters", async () => {
    let receivedDiganostics: Promise<Protocol.PublishDiagnosticsParams> =
      new Promise((resolve, _reject) =>
        languageServer.onNotification(
          Protocol.PublishDiagnosticsNotification.type,
          (params) => {
            resolve(params);
          },
        ),
      );
    openDocument(outdent`
      let () =
        let x = 123 in
        ()
    `);
    const params = await receivedDiganostics;

    expect(params.uri).toBe(uri);
  });
});
