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
          testUri("file.ml"),
          "ocaml",
          0,
          source,
        ),
      },
    );
  }

  async function queryDefinition(position: Types.Position) {
    let result = await languageServer.sendRequest(
      Protocol.TypeDefinitionRequest.type,
      {
        textDocument: Types.TextDocumentIdentifier.create(testUri("file.ml")),
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

  it("returns location of a type definition", async () => {
    openDocument(outdent`
      (* type we are jumping on *)
      type t = T of int

      let x = T 43
    `);

    let result = await queryDefinition(Types.Position.create(3, 4));

    expect(result.length).toBe(1);
    expect(result[0].range).toMatchInlineSnapshot(`
Object {
  "end": Object {
    "character": 5,
    "line": 1,
  },
  "start": Object {
    "character": 5,
    "line": 1,
  },
}
`);
    expect(result[0].uri).toEqualUri(testUri("file.ml"));
  });

  it("ignores names in values namespace", async () => {
    openDocument(outdent`
      (* type we are jumping on *)
      type t = T of int

      let t = T 42
      let x = T 43
    `);

    let result = await queryDefinition(Types.Position.create(4, 4));

    expect(result.length).toBe(1);
    expect(result[0].range).toMatchInlineSnapshot(`
Object {
  "end": Object {
    "character": 5,
    "line": 1,
  },
  "start": Object {
    "character": 5,
    "line": 1,
  },
}
`);
    expect(result[0].uri).toEqualUri(testUri("file.ml"));
  });

  it("ignores names in values namespace (cursor on same named value)", async () => {
    openDocument(outdent`
      (* type we are jumping on *)
      type t = T of int

      let t = T 42
    `);

    let result = await queryDefinition(Types.Position.create(3, 4));

    expect(result.length).toBe(1);
    expect(result[0].range).toMatchInlineSnapshot(`
Object {
  "end": Object {
    "character": 5,
    "line": 1,
  },
  "start": Object {
    "character": 5,
    "line": 1,
  },
}
`);
    expect(result[0].uri).toEqualUri(testUri("file.ml"));
  });
});
