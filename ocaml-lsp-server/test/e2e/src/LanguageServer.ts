import * as cp from "child_process";
import * as os from "os";
import * as fs from "fs";
import * as path from "path";
import * as rpc from "vscode-jsonrpc";

import * as Protocol from "vscode-languageserver-protocol";
import * as Rpc from "vscode-jsonrpc";

let serverBin =
  os.platform() === "win32" ? "ocaml-lsp-server.exe" : "ocaml-lsp-server";

let serverPath = path.join(
  __dirname,
  "..",
  "..",
  "..",
  "..",
  "_build",
  "install",
  "default",
  "bin",
  serverBin
);

if (!fs.existsSync(serverPath)) {
  console.error(`Unable to ocaml-lsp-server executable at: ${serverPath}`);
  process.exit(1);
}

export type LanguageServer = Rpc.MessageConnection;

let prefix = process.platform === "win32" ? "file:///" : "file://";

export const toURI = s => {
  return prefix + s;
};

export const start = (opts?: cp.SpawnOptions) => {
  opts = opts || {
    env: { ...process.env, MERLIN_LOG: "-" }
  };
  let childProcess = cp.spawn(serverPath, [], opts);

  let connection = rpc.createMessageConnection(
    new rpc.StreamMessageReader(childProcess.stdout),
    new rpc.StreamMessageWriter(childProcess.stdin)
  );

  if (process.env.MERLIN_LSP_TEST_DEBUG) {
    childProcess.stderr.on("data", d => {
      console.log("Received data: " + d);
    });
  }

  connection.listen();

  return connection as LanguageServer;
};

export const startAndInitialize = async (
  capabilities?: Partial<Protocol.ClientCapabilities>
) => {
  let languageServer = start();

  capabilities = capabilities || {};

  let initializeParameters: Protocol.InitializeParams = {
    processId: process.pid,
    rootUri: toURI(path.join(__dirname, "..")),
    capabilities: capabilities,
    workspaceFolders: []
  };

  let result = await languageServer.sendRequest(
    Protocol.InitializeRequest.type,
    initializeParameters
  );
  return languageServer;
};

export const exit = async languageServer => {
  let ret = new Promise((resolve, reject) => {
    languageServer.onClose(() => {
      languageServer.dispose();
      resolve();
    });
  });

  let notification = new rpc.NotificationType<string, void>("exit");
  languageServer.sendNotification(notification);

  return ret;
};
