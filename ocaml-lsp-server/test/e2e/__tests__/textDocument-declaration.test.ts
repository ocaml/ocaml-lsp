import { promises as fs } from "fs";
import * as path from "path";
import * as child_process from "child_process";
import * as LanguageServer from "./../src/LanguageServer";
import * as Types from "vscode-languageserver-types";
import { testUri } from "./../src/LanguageServer";
import * as Protocol from "vscode-languageserver-protocol";
import { isNotNullable } from "../src/utils";

describe("textDocument/declaration", () => {
  let languageServer: LanguageServer.LanguageServer;

  let testWorkspacePath = path.join(__dirname, "declaration_files/");

  let createPathForFile = (filename: string) =>
    path.join(testWorkspacePath, filename);

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
  });

  async function openDocument(filepath: string) {
    let source = await fs.readFile(filepath);
    languageServer.sendNotification(
      Protocol.DidOpenTextDocumentNotification.type,
      {
        textDocument: Types.TextDocumentItem.create(
          testUri(filepath),
          "ocaml",
          0,
          source.toString(),
        ),
      },
    );
  }

  async function queryDeclaration(filepath: string, position: Types.Position) {
    let result = await languageServer.sendRequest(
      Protocol.DeclarationRequest.type,
      {
        textDocument: Types.TextDocumentIdentifier.create(testUri(filepath)),
        position,
      },
    );

    if (result === null) return [];

    result = Array.isArray(result) ? result : [result];

    return result
      .map((location) => (Types.Location.is(location) ? location : null))
      .filter(isNotNullable);
  }

  it("returns location of a declaration", async () => {
    child_process.execSync("dune build", { cwd: testWorkspacePath });

    await openDocument(createPathForFile("main.ml"));

    let result = await queryDeclaration(
      createPathForFile("main.ml"),
      Types.Position.create(0, 13),
    );

    expect(result.length).toBe(1);
    expect(result[0].range).toMatchInlineSnapshot(`
Object {
  "end": Object {
    "character": 4,
    "line": 0,
  },
  "start": Object {
    "character": 4,
    "line": 0,
  },
}
`);
    expect(result[0].uri).toEqualUri(testUri(createPathForFile("lib.mli")));
  });
});
