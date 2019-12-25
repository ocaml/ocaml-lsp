import * as cp from "child_process";
import * as os from "os";
import * as fs from "fs";
import * as path from "path";
import * as rpc from "vscode-jsonrpc";

import * as Protocol from "vscode-languageserver-protocol";
import * as Rpc from "vscode-jsonrpc";

let serverBin = os.platform() === "win32" ? "ocamllsp.exe" : "ocamllsp";

export type LanguageServer = Rpc.MessageConnection;

let prefix = process.platform === "win32" ? "file:///" : "file://";

export const toURI = s => {
  return prefix + s;
};

export const start = (opts?: cp.SpawnOptions) => {
  opts = opts || {
    env: { ...process.env, OCAML_LSP_SERVER_LOG: "-" },
  };
  let childProcess = cp.spawn(serverBin, [], opts);

  let connection = rpc.createMessageConnection(
    new rpc.StreamMessageReader(childProcess.stdout),
    new rpc.StreamMessageWriter(childProcess.stdin),
  );

  childProcess.stderr.on("data", d => {
    if (process.env.MERLIN_LSP_TEST_DEBUG) {
      console.log("Received data: " + d);
    }
  });

  connection.listen();

  return connection as LanguageServer;
};

export const startAndInitialize = async (
  capabilities?: Partial<Protocol.ClientCapabilities>,
) => {
  let languageServer = start();

  capabilities = capabilities || {};

  let initializeParameters: Protocol.InitializeParams = {
    processId: process.pid,
    rootUri: toURI(path.join(__dirname, "..")),
    capabilities: capabilities,
    workspaceFolders: [],
  };

  let result = await languageServer.sendRequest(
    Protocol.InitializeRequest.type,
    initializeParameters,
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
