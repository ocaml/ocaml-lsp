import * as path from "path";
import * as child_process from "child_process";
import * as Types from "vscode-languageserver-types";
import * as Protocol from "vscode-languageserver-protocol";
import * as LanguageServer from "./../src/LanguageServer";

let createWorkspaces = (names: string[]) => {
  let createSingleWorkspace = (name: string) => {
    let workspacePath = path.join(__dirname, name);
    let uri = LanguageServer.toURI(workspacePath);
    let folder: Protocol.WorkspaceFolder = { name, uri };
    return {
      name,
      path: workspacePath,
      uri,
      folder,
    };
  };

  let workspaces = names.map(createSingleWorkspace);

  let toTestResult = (symbol: Types.SymbolInformation) =>
    `${symbol.name} ${symbol.kind} ${workspaces.reduce(
      (uri, { path, name }) => uri.replace(path, `<${name}>`),
      symbol.location.uri,
    )} ${symbol.location.range.start.line}:${
      symbol.location.range.start.character
    } ${symbol.location.range.end.line}:${symbol.location.range.end.character}`;

  return {
    workspaces,
    toTestResult,
  };
};

let buildProject = (cwd : string): void => {
  child_process.execSync("dune build --root . @check", { cwd: cwd });
}

describe("workspace/symbol", () => {
  let languageServer: Protocol.MessageConnection = null;

  let {
    workspaces: [workspaceA, workspaceB],
    toTestResult,
  } = createWorkspaces(["workspace_symbol_A", "workspace_symbol_B"]);

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
    languageServer = null;
  });

  async function queryWorkspaceSymbol(params: Protocol.WorkspaceSymbolParams) {
    return await languageServer.sendRequest(
      Protocol.WorkspaceSymbolRequest.type,
      params,
    );
  }

  it("returns all symbols from workspace", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      workspaceFolders: [workspaceA.folder],
    });

    buildProject(workspaceA.path);

    let symbols: Types.SymbolInformation[] = await queryWorkspaceSymbol({
      query: "",
    });

    expect(symbols.map(toTestResult)).toMatchInlineSnapshot(`
      Array [
        "a_x 12 file://<workspace_symbol_A>/bin/a.ml 0:4 0:7",
        "A_B 2 file://<workspace_symbol_A>/bin/a.ml 2:0 5:3",
        "a_d 12 file://<workspace_symbol_A>/bin/a.ml 7:4 7:7",
        "user 15 file://<workspace_symbol_A>/bin/a.ml 12:0 14:12",
        "a_u 12 file://<workspace_symbol_A>/bin/a.ml 16:4 16:7",
        "a_arr 12 file://<workspace_symbol_A>/bin/a.ml 18:4 18:9",
        "a_m 12 file://<workspace_symbol_A>/bin/a.ml 20:4 20:7",
        "a_n 12 file://<workspace_symbol_A>/bin/a.ml 20:9 20:12",
        "a_i 12 file://<workspace_symbol_A>/bin/a.ml 22:4 22:7",
        "StringMap 2 file://<workspace_symbol_A>/bin/a.ml 26:0 26:36",
        "My_string 2 file://<workspace_symbol_A>/bin/a.ml 27:0 27:25",
        "A_Mod 2 file://<workspace_symbol_A>/bin/a.ml 29:0 39:3",
        "X_int 2 file://<workspace_symbol_A>/bin/a.ml 41:0 43:3",
        "Increment 2 file://<workspace_symbol_A>/bin/a.ml 45:0 47:3",
        "Foo 9 file://<workspace_symbol_A>/bin/a.ml 49:0 49:23",
        "stack_of_ints 5 file://<workspace_symbol_A>/bin/a.ml 51:0 65:5",
        "stack_of_ints 5 file://<workspace_symbol_A>/bin/a.ml 51:0 65:5",
        "stack_of_ints 15 file://<workspace_symbol_A>/bin/a.ml 51:0 65:5",
        "#stack_of_ints 15 file://<workspace_symbol_A>/bin/a.ml 51:0 65:5",
        "main_y 12 file://<workspace_symbol_A>/bin/main.ml 0:4 0:10",
        "user 15 file://<workspace_symbol_A>/lib/lib.ml 3:0 5:1",
        "lib_x 12 file://<workspace_symbol_A>/lib/lib.ml 6:4 6:9",
        "hd 12 file://<workspace_symbol_A>/lib/lib.ml 8:4 8:6",
        "lib_private_fn 12 file://<workspace_symbol_A>/lib/lib.ml 10:4 10:18",
        "lib_type 12 file://<workspace_symbol_A>/lib/lib.ml 12:4 12:12",
        "t 15 file://<workspace_symbol_A>/lib/LibTypes.mli 0:0 0:15",
      ]
    `);
  });

  it("returns filtered symbols from workspace", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      workspaceFolders: [workspaceA.folder],
    });

    buildProject(workspaceA.path);

    let symbols: Types.SymbolInformation[] = await queryWorkspaceSymbol({
      query: "a_",
    });

    expect(symbols.map(toTestResult)).toMatchInlineSnapshot(`
      Array [
        "a_x 12 file://<workspace_symbol_A>/bin/a.ml 0:4 0:7",
        "a_d 12 file://<workspace_symbol_A>/bin/a.ml 7:4 7:7",
        "a_u 12 file://<workspace_symbol_A>/bin/a.ml 16:4 16:7",
        "a_arr 12 file://<workspace_symbol_A>/bin/a.ml 18:4 18:9",
        "a_m 12 file://<workspace_symbol_A>/bin/a.ml 20:4 20:7",
        "a_n 12 file://<workspace_symbol_A>/bin/a.ml 20:9 20:12",
        "a_i 12 file://<workspace_symbol_A>/bin/a.ml 22:4 22:7",
      ]
    `);
  });

  it("handles mutiple wokspaces", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      workspaceFolders: [workspaceA.folder, workspaceB.folder],
    });


    buildProject(workspaceA.path);
    buildProject(workspaceB.path);

    let symbols: Types.SymbolInformation[] = await queryWorkspaceSymbol({
      query: "",
    });

    expect(symbols.map(toTestResult)).toMatchInlineSnapshot(`
      Array [
        "a_x 12 file://<workspace_symbol_A>/bin/a.ml 0:4 0:7",
        "A_B 2 file://<workspace_symbol_A>/bin/a.ml 2:0 5:3",
        "a_d 12 file://<workspace_symbol_A>/bin/a.ml 7:4 7:7",
        "user 15 file://<workspace_symbol_A>/bin/a.ml 12:0 14:12",
        "a_u 12 file://<workspace_symbol_A>/bin/a.ml 16:4 16:7",
        "a_arr 12 file://<workspace_symbol_A>/bin/a.ml 18:4 18:9",
        "a_m 12 file://<workspace_symbol_A>/bin/a.ml 20:4 20:7",
        "a_n 12 file://<workspace_symbol_A>/bin/a.ml 20:9 20:12",
        "a_i 12 file://<workspace_symbol_A>/bin/a.ml 22:4 22:7",
        "StringMap 2 file://<workspace_symbol_A>/bin/a.ml 26:0 26:36",
        "My_string 2 file://<workspace_symbol_A>/bin/a.ml 27:0 27:25",
        "A_Mod 2 file://<workspace_symbol_A>/bin/a.ml 29:0 39:3",
        "X_int 2 file://<workspace_symbol_A>/bin/a.ml 41:0 43:3",
        "Increment 2 file://<workspace_symbol_A>/bin/a.ml 45:0 47:3",
        "Foo 9 file://<workspace_symbol_A>/bin/a.ml 49:0 49:23",
        "stack_of_ints 5 file://<workspace_symbol_A>/bin/a.ml 51:0 65:5",
        "stack_of_ints 5 file://<workspace_symbol_A>/bin/a.ml 51:0 65:5",
        "stack_of_ints 15 file://<workspace_symbol_A>/bin/a.ml 51:0 65:5",
        "#stack_of_ints 15 file://<workspace_symbol_A>/bin/a.ml 51:0 65:5",
        "main_y 12 file://<workspace_symbol_A>/bin/main.ml 0:4 0:10",
        "user 15 file://<workspace_symbol_A>/lib/lib.ml 3:0 5:1",
        "lib_x 12 file://<workspace_symbol_A>/lib/lib.ml 6:4 6:9",
        "hd 12 file://<workspace_symbol_A>/lib/lib.ml 8:4 8:6",
        "lib_private_fn 12 file://<workspace_symbol_A>/lib/lib.ml 10:4 10:18",
        "lib_type 12 file://<workspace_symbol_A>/lib/lib.ml 12:4 12:12",
        "t 15 file://<workspace_symbol_A>/lib/LibTypes.mli 0:0 0:15",
        "workspace_B 12 file://<workspace_symbol_B>/main.ml 0:4 0:15",
      ]
    `);
  });

  it("shows a notification message if no build directory found", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      workspaceFolders: [workspaceA.folder, workspaceB.folder],
    });

    child_process.execSync("dune clean", { cwd: workspaceA.path });
    child_process.execSync("dune clean", { cwd: workspaceB.path });

    type StarNotificationHandlerParams = Parameters<Protocol.StarNotificationHandler>;
    let receivedNotification: Promise<{
      method: StarNotificationHandlerParams[0];
      params: StarNotificationHandlerParams[1];
    }> = new Promise((resolve, _reject) =>
      languageServer.onNotification((method, params) =>
        resolve({ method, params }),
      ),
    );

    await queryWorkspaceSymbol({
      query: "",
    });

    let notification = await receivedNotification;
    expect(notification.method).toBe(
      Protocol.ShowMessageNotification.type.method,
    );
    // @ts-ignore
    expect(notification.params.type).toEqual(Protocol.MessageType.Warning);
    // @ts-ignore
    expect(notification.params.message).toBe(
      `No build directory found in workspace(s): ${workspaceA.name}, ${workspaceB.name}`,
    );
  });
});
