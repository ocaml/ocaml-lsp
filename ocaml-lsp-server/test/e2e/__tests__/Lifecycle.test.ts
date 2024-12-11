import * as Protocol from "vscode-languageserver-protocol";
import * as LanguageServer from "./../src/LanguageServer";

test("basic", async () => {
  const languageServer = LanguageServer.start();
  await LanguageServer.exit(languageServer);
});

test("initialize with empty capabilities", async () => {
  const languageServer = LanguageServer.start();

  const capabilities: Protocol.ClientCapabilities = {};

  const initializeParameters: Protocol.InitializeParams = {
    processId: process.pid,
    rootUri: LanguageServer.toURI(__dirname),
    capabilities: capabilities,
    workspaceFolders: [],
  };

  const result = await languageServer.sendRequest(
    Protocol.InitializeRequest.type,
    initializeParameters,
  );

  expect(result.capabilities).toBeTruthy();
  await LanguageServer.exit(languageServer);
});
