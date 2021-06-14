import outdent from "outdent";
import * as LanguageServer from "../src/LanguageServer";

import * as Types from "vscode-languageserver-types";

describe("ocamllsp/nextHole", () => {
  let languageServer = null;

  async function openDocument(source) {
    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test.ml",
        "ocaml",
        0,
        source,
      ),
    });
  }

  function currPos(line, character) {
    return {
      line,
      character,
    };
  }

  async function sendNextHoleReq(position) {
    return languageServer.sendRequest("ocamllsp/nextHole", {
      uri: "file:///test.ml",
      position,
    });
  }

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
    languageServer = null;
  });

  it("null when no holes in file", async () => {
    await openDocument(
      outdent`
let u = 1
`,
    );

    let r = await sendNextHoleReq(currPos(1, 1));
    expect(r).toMatchInlineSnapshot(`null`);
  });

  it("one hole", async () => {
    await openDocument(
      outdent`
let k = match () with () -> _
`,
    );

    let r = await sendNextHoleReq(currPos(0, 3));
    expect(r).toMatchInlineSnapshot(`
      Object {
        "end": Object {
          "character": 29,
          "line": 0,
        },
        "start": Object {
          "character": 28,
          "line": 0,
        },
      }
    `);
  });

  it("several holes", async () => {
    await openDocument(
      outdent`
let u =
  let i = match Some 1 with None -> _ | Some -> _ in
  let b = match () with () -> _ in
  ()
      `,
    );
    let r0 = await sendNextHoleReq(currPos(0, 1));
    expect(r0).toMatchInlineSnapshot(`
      Object {
        "end": Object {
          "character": 37,
          "line": 1,
        },
        "start": Object {
          "character": 36,
          "line": 1,
        },
      }
    `);

    let r1 = await sendNextHoleReq(currPos(1, 36));
    expect(r1).toMatchInlineSnapshot(`
      Object {
        "end": Object {
          "character": 49,
          "line": 1,
        },
        "start": Object {
          "character": 48,
          "line": 1,
        },
      }
    `);

    let r2 = await sendNextHoleReq(currPos(1, 49));
    expect(r2).toMatchInlineSnapshot(`
      Object {
        "end": Object {
          "character": 31,
          "line": 2,
        },
        "start": Object {
          "character": 30,
          "line": 2,
        },
      }
    `);
  });
});
