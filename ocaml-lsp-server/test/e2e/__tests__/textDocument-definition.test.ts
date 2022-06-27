import outdent from "outdent";
import * as LanguageServer from "./../src/LanguageServer";
import * as Types from "vscode-languageserver-types";
import { testUri } from "./../src/LanguageServer";
import * as Protocol from "vscode-languageserver-protocol";
import { isNotNullable } from "../src/utils";

describe("textDocument/definition", () => {
  let languageServer: LanguageServer.LanguageServer;

  function openDocument(source: string) {
    languageServer.sendNotification(
      Protocol.DidOpenTextDocumentNotification.type,
      {
        textDocument: Types.TextDocumentItem.create(
          testUri("test.ml"),
          "ocaml",
          0,
          source,
        ),
      },
    );
  }

  async function queryDefinition(position: Types.Position) {
    let result = await languageServer.sendRequest(
      Protocol.DefinitionRequest.type,
      {
        textDocument: Types.TextDocumentIdentifier.create(testUri("test.ml")),
        position,
      },
    );

    if (result === null) return [];

    result = Array.isArray(result) ? result : [result];

    return result
      .map((location) => (Types.Location.is(location) ? location : null))
      .filter(isNotNullable);
  }

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
  });

  it("returns location of a definition", async () => {
    openDocument(outdent`
      let x = 43

      let () =
        print_int x
    `);

    let result = await queryDefinition(Types.Position.create(3, 12));

    expect(result.length).toBe(1);
    expect(result[0].range).toMatchObject({
      end: { character: 4, line: 0 },
      start: { character: 4, line: 0 },
    });
    expect(result[0].uri).toEqualUri(testUri("test.ml"));
  });
});
