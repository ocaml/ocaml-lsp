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

let buildProject = (cwd: string): void => {
  child_process.execSync("dune build --root . @check", { cwd: cwd });
};

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
        "stack_of_ints 5 file://<workspace_symbol_A>/bin/a.ml 51:0 65:5",
        "size 6 file://<workspace_symbol_A>/bin/a.ml 64:11 64:15",
        "peek 6 file://<workspace_symbol_A>/bin/a.ml 62:11 62:15",
        "pop 6 file://<workspace_symbol_A>/bin/a.ml 57:11 57:14",
        "push 6 file://<workspace_symbol_A>/bin/a.ml 55:11 55:15",
        "the_list 12 file://<workspace_symbol_A>/bin/a.ml 53:16 53:24",
        "Foo 9 file://<workspace_symbol_A>/bin/a.ml 49:0 49:23",
        "Increment 2 file://<workspace_symbol_A>/bin/a.ml 45:0 47:3",
        "increment_x 12 file://<workspace_symbol_A>/bin/a.ml 46:2 46:27",
        "X_int 2 file://<workspace_symbol_A>/bin/a.ml 41:0 43:3",
        "x 12 file://<workspace_symbol_A>/bin/a.ml 42:2 42:13",
        "A_Mod 2 file://<workspace_symbol_A>/bin/a.ml 29:0 39:3",
        "compare 12 file://<workspace_symbol_A>/bin/a.ml 38:2 38:30",
        "private_mod_fn 12 file://<workspace_symbol_A>/bin/a.ml 36:2 36:33",
        "t 15 file://<workspace_symbol_A>/bin/a.ml 34:2 34:14",
        "My_string 2 file://<workspace_symbol_A>/bin/a.ml 27:0 27:25",
        "StringMap 2 file://<workspace_symbol_A>/bin/a.ml 26:0 26:36",
        "a_i 12 file://<workspace_symbol_A>/bin/a.ml 22:0 24:7",
        "a_arr 12 file://<workspace_symbol_A>/bin/a.ml 18:0 18:14",
        "a_u 12 file://<workspace_symbol_A>/bin/a.ml 16:0 16:15",
        "user 15 file://<workspace_symbol_A>/bin/a.ml 12:0 14:12",
        "NotAdmin 9 file://<workspace_symbol_A>/bin/a.ml 14:2 14:12",
        "Admin 9 file://<workspace_symbol_A>/bin/a.ml 13:2 13:9",
        "a_d 12 file://<workspace_symbol_A>/bin/a.ml 7:0 10:14",
        "A_B 2 file://<workspace_symbol_A>/bin/a.ml 2:0 5:3",
        "a_b 12 file://<workspace_symbol_A>/bin/a.ml 4:2 4:19",
        "a_b_t 15 file://<workspace_symbol_A>/bin/a.ml 3:2 3:21",
        "a_x 12 file://<workspace_symbol_A>/bin/a.ml 0:0 0:11",
        "main_y 12 file://<workspace_symbol_A>/bin/main.ml 0:0 0:22",
        "lib_type 12 file://<workspace_symbol_A>/lib/lib.ml 12:0 12:38",
        "lib_private_fn 12 file://<workspace_symbol_A>/lib/lib.ml 10:0 10:38",
        "hd 12 file://<workspace_symbol_A>/lib/lib.ml 8:0 8:16",
        "lib_x 12 file://<workspace_symbol_A>/lib/lib.ml 6:0 6:14",
        "user 15 file://<workspace_symbol_A>/lib/lib.ml 3:0 5:1",
        "name 7 file://<workspace_symbol_A>/lib/lib.ml 4:2 4:14",
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
        "a_i 12 file://<workspace_symbol_A>/bin/a.ml 22:0 24:7",
        "a_arr 12 file://<workspace_symbol_A>/bin/a.ml 18:0 18:14",
        "a_u 12 file://<workspace_symbol_A>/bin/a.ml 16:0 16:15",
        "a_d 12 file://<workspace_symbol_A>/bin/a.ml 7:0 10:14",
        "a_b 12 file://<workspace_symbol_A>/bin/a.ml 4:2 4:19",
        "a_b_t 15 file://<workspace_symbol_A>/bin/a.ml 3:2 3:21",
        "a_x 12 file://<workspace_symbol_A>/bin/a.ml 0:0 0:11",
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
        "stack_of_ints 5 file://<workspace_symbol_A>/bin/a.ml 51:0 65:5",
        "size 6 file://<workspace_symbol_A>/bin/a.ml 64:11 64:15",
        "peek 6 file://<workspace_symbol_A>/bin/a.ml 62:11 62:15",
        "pop 6 file://<workspace_symbol_A>/bin/a.ml 57:11 57:14",
        "push 6 file://<workspace_symbol_A>/bin/a.ml 55:11 55:15",
        "the_list 12 file://<workspace_symbol_A>/bin/a.ml 53:16 53:24",
        "Foo 9 file://<workspace_symbol_A>/bin/a.ml 49:0 49:23",
        "Increment 2 file://<workspace_symbol_A>/bin/a.ml 45:0 47:3",
        "increment_x 12 file://<workspace_symbol_A>/bin/a.ml 46:2 46:27",
        "X_int 2 file://<workspace_symbol_A>/bin/a.ml 41:0 43:3",
        "x 12 file://<workspace_symbol_A>/bin/a.ml 42:2 42:13",
        "A_Mod 2 file://<workspace_symbol_A>/bin/a.ml 29:0 39:3",
        "compare 12 file://<workspace_symbol_A>/bin/a.ml 38:2 38:30",
        "private_mod_fn 12 file://<workspace_symbol_A>/bin/a.ml 36:2 36:33",
        "t 15 file://<workspace_symbol_A>/bin/a.ml 34:2 34:14",
        "My_string 2 file://<workspace_symbol_A>/bin/a.ml 27:0 27:25",
        "StringMap 2 file://<workspace_symbol_A>/bin/a.ml 26:0 26:36",
        "a_i 12 file://<workspace_symbol_A>/bin/a.ml 22:0 24:7",
        "a_arr 12 file://<workspace_symbol_A>/bin/a.ml 18:0 18:14",
        "a_u 12 file://<workspace_symbol_A>/bin/a.ml 16:0 16:15",
        "user 15 file://<workspace_symbol_A>/bin/a.ml 12:0 14:12",
        "NotAdmin 9 file://<workspace_symbol_A>/bin/a.ml 14:2 14:12",
        "Admin 9 file://<workspace_symbol_A>/bin/a.ml 13:2 13:9",
        "a_d 12 file://<workspace_symbol_A>/bin/a.ml 7:0 10:14",
        "A_B 2 file://<workspace_symbol_A>/bin/a.ml 2:0 5:3",
        "a_b 12 file://<workspace_symbol_A>/bin/a.ml 4:2 4:19",
        "a_b_t 15 file://<workspace_symbol_A>/bin/a.ml 3:2 3:21",
        "a_x 12 file://<workspace_symbol_A>/bin/a.ml 0:0 0:11",
        "main_y 12 file://<workspace_symbol_A>/bin/main.ml 0:0 0:22",
        "lib_type 12 file://<workspace_symbol_A>/lib/lib.ml 12:0 12:38",
        "lib_private_fn 12 file://<workspace_symbol_A>/lib/lib.ml 10:0 10:38",
        "hd 12 file://<workspace_symbol_A>/lib/lib.ml 8:0 8:16",
        "lib_x 12 file://<workspace_symbol_A>/lib/lib.ml 6:0 6:14",
        "user 15 file://<workspace_symbol_A>/lib/lib.ml 3:0 5:1",
        "name 7 file://<workspace_symbol_A>/lib/lib.ml 4:2 4:14",
        "t 15 file://<workspace_symbol_A>/lib/LibTypes.mli 0:0 0:15",
        "workspace_B 12 file://<workspace_symbol_B>/main.ml 0:0 0:31",
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
