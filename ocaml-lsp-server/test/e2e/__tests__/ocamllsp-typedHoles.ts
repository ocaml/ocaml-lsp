import outdent from "outdent";
import * as LanguageServer from "../src/LanguageServer";
import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";

describe("ocamllsp/typedHoles", () => {
  let languageServer: LanguageServer.LanguageServer;

  function openDocument(source: string) {
    languageServer.sendNotification(
      Protocol.DidOpenTextDocumentNotification.type,
      {
        textDocument: Types.TextDocumentItem.create(
          "file:///test.ml",
          "ocaml",
          0,
          source,
        ),
      },
    );
  }

  async function sendTypedHolesReq() {
    return languageServer.sendRequest("ocamllsp/typedHoles", {
      uri: "file:///test.ml",
    });
  }

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
  });

  it("empty when no holes in file", async () => {
    openDocument(
      outdent`
let u = 1
`,
    );

    let r = await sendTypedHolesReq();
    expect(r).toMatchInlineSnapshot(`Array []`);
  });

  it("one hole", async () => {
    openDocument(
      outdent`
let k = match () with () -> _
`,
    );

    let r = await sendTypedHolesReq();
    expect(r).toMatchInlineSnapshot(`
      Array [
        Object {
          "end": Object {
            "character": 29,
            "line": 0,
          },
          "start": Object {
            "character": 28,
            "line": 0,
          },
        },
      ]
    `);
  });

  it("several holes", async () => {
    openDocument(
      outdent`
let u =
  let i = match Some 1 with None -> _ | Some -> _ in
  let b = match () with () -> _ in
  ()
      `,
    );
    let r = await sendTypedHolesReq();
    expect(r).toMatchInlineSnapshot(`
      Array [
        Object {
          "end": Object {
            "character": 31,
            "line": 2,
          },
          "start": Object {
            "character": 30,
            "line": 2,
          },
        },
        Object {
          "end": Object {
            "character": 37,
            "line": 1,
          },
          "start": Object {
            "character": 36,
            "line": 1,
          },
        },
        Object {
          "end": Object {
            "character": 49,
            "line": 1,
          },
          "start": Object {
            "character": 48,
            "line": 1,
          },
        },
      ]
    `);
  });
});
