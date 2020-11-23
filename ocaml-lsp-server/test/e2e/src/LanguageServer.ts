import * as cp from "child_process";
import * as os from "os";
import * as path from "path";
import * as rpc from "vscode-jsonrpc";

import * as Protocol from "vscode-languageserver-protocol";
import * as Rpc from "vscode-jsonrpc";
import { URI } from "vscode-uri";

const ocamlVersion = cp.execSync("ocamlc --version").toString();
export function ocamlVersionGEq(versString: string) {
  return ocamlVersion >= versString;
}

let serverBin = os.platform() === "win32" ? "ocamllsp.exe" : "ocamllsp";

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
  serverBin,
);

export type LanguageServer = Rpc.MessageConnection;

let prefix = process.platform === "win32" ? "file:///" : "file://";

export const toURI = (s) => {
  return prefix + s;
};

export const start = (opts?: cp.SpawnOptions) => {
  opts = opts || {
    env: { ...process.env, OCAML_LSP_SERVER_LOG: "-" },
  };
  let childProcess = cp.spawn(serverPath, [], opts);

  let connection = rpc.createMessageConnection(
    new rpc.StreamMessageReader(childProcess.stdout),
    new rpc.StreamMessageWriter(childProcess.stdin),
  );

  childProcess.stderr.on("data", (d) => {
    if (process.env.OCAMLLSP_TEST_DEBUG) {
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

  await languageServer.sendRequest(
    Protocol.InitializeRequest.type,
    initializeParameters,
  );
  return languageServer;
};

export const exit = async (languageServer: rpc.MessageConnection) => {
  let ret = new Promise((resolve, _reject) => {
    languageServer.onClose(() => {
      languageServer.dispose();
      resolve();
    });
  });

  let notification = new rpc.NotificationType<string, void>("exit");
  languageServer.sendNotification(notification);

  return ret;
};

export const testUri = (file: string) => {
  return URI.file(file).toString();
};

export const toEqualUri = (received: string, expected: string) => {
  const options = {
    comment: "Uri equality",
    isNot: this.isNot,
    promise: this.promise,
  };
  const pass =
    URI.parse(received).toString() === URI.parse(received).toString();
  const message = pass
    ? () =>
        this.utils.matcherHint("toEqualUri", undefined, undefined, options) +
        "\n\n" +
        `Expected: not ${this.utils.printExpected(expected)}\n` +
        `Received: ${this.utils.printReceived(received)}`
    : () =>
        this.utils.matcherHint("toBe", undefined, undefined, options) +
        "\n\n" +
        `Expected: ${this.utils.printExpected(expected)}\n` +
        `Received: ${this.utils.printReceived(received)}`;
  return { pass, message };
};
