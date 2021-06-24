import outdent from "outdent";
import * as path from "path";
import * as LanguageServer from "../src/LanguageServer";

import * as Types from "vscode-languageserver-types";

function findAnnotateAction(actions) {
  return actions.find((action) => action.kind == "type-annotate");
}

function findInferredAction(actions) {
  return actions.find((action) => action.kind == "inferred_intf");
}

describe("textDocument/codeAction", () => {
  let languageServer = null;

  async function openDocument(source, uri) {
    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(uri, "ocaml", 0, source),
    });
  }

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
    languageServer = null;
  });

  async function codeAction(uri, start, end) {
    return await languageServer.sendRequest("textDocument/codeAction", {
      textDocument: Types.TextDocumentIdentifier.create(uri),
      context: { diagnostics: [] },
      range: { start, end },
    });
  }

  it("can destruct sum types", async () => {
    await openDocument(
      outdent`
type t = Foo of int | Bar of bool

let f (x : t) = x
`,
      "file:///test.ml",
    );
    let start = Types.Position.create(2, 16);
    let end = Types.Position.create(2, 17);
    let actions = await codeAction("file:///test.ml", start, end);
    expect(actions).toMatchInlineSnapshot(`
      Array [
        Object {
          "command": Object {
            "arguments": Array [
              Object {
                "position": Object {
                  "character": 16,
                  "line": 2,
                },
              },
            ],
            "command": "ocaml.next-hole",
            "title": "Jump to Next Hole",
          },
          "edit": Object {
            "documentChanges": Array [
              Object {
                "edits": Array [
                  Object {
                    "newText": "match x with | Foo _ -> _ | Bar _ -> _",
                    "range": Object {
                      "end": Object {
                        "character": 17,
                        "line": 2,
                      },
                      "start": Object {
                        "character": 16,
                        "line": 2,
                      },
                    },
                  },
                ],
                "textDocument": Object {
                  "uri": "file:///test.ml",
                  "version": 0,
                },
              },
            ],
          },
          "isPreferred": false,
          "kind": "destruct",
          "title": "Destruct",
        },
        Object {
          "edit": Object {
            "documentChanges": Array [
              Object {
                "edits": Array [
                  Object {
                    "newText": "(x : t)",
                    "range": Object {
                      "end": Object {
                        "character": 17,
                        "line": 2,
                      },
                      "start": Object {
                        "character": 16,
                        "line": 2,
                      },
                    },
                  },
                ],
                "textDocument": Object {
                  "uri": "file:///test.ml",
                  "version": 0,
                },
              },
            ],
          },
          "isPreferred": false,
          "kind": "type-annotate",
          "title": "Type-annotate",
        },
      ]
    `);
  });

  it("can infer module interfaces", async () => {
    await openDocument(
      outdent`
type t = Foo of int | Bar of bool

let f (x : t) = x
`,
      "file:///test.ml",
    );
    await openDocument("", "file:///test.mli");
    let start = Types.Position.create(0, 0);
    let end = Types.Position.create(0, 0);
    let actions = await codeAction("file:///test.mli", start, end);
    expect(findInferredAction(actions)).toMatchInlineSnapshot(`
      Object {
        "edit": Object {
          "documentChanges": Array [
            Object {
              "edits": Array [
                Object {
                  "newText": "type t = Foo of int | Bar of bool
      val f : t -> t
      ",
                  "range": Object {
                    "end": Object {
                      "character": 0,
                      "line": 0,
                    },
                    "start": Object {
                      "character": 0,
                      "line": 0,
                    },
                  },
                },
              ],
              "textDocument": Object {
                "uri": "file:///test.mli",
                "version": 0,
              },
            },
          ],
        },
        "isPreferred": false,
        "kind": "inferred_intf",
        "title": "Insert inferred interface",
      }
    `);
  });

  it("opens the implementation if not in store", async () => {
    let testWorkspacePath = path.join(__dirname, "declaration_files/");
    let intfFilepath = path.join(testWorkspacePath, "lib.mli");
    let intfUri = "file://" + intfFilepath;
    await openDocument("", intfUri);
    let start = Types.Position.create(0, 0);
    let end = Types.Position.create(0, 0);
    let actions = await codeAction(intfUri, start, end);
    expect(findInferredAction(actions).edit.documentChanges.map((a) => a.edits))
      .toMatchInlineSnapshot(`
      Array [
        Array [
          Object {
            "newText": "val x : int
      ",
            "range": Object {
              "end": Object {
                "character": 0,
                "line": 0,
              },
              "start": Object {
                "character": 0,
                "line": 0,
              },
            },
          },
        ],
      ]
    `);
  });

  it("can type-annotate a function argument", async () => {
    await openDocument(
      outdent`
type t = Foo of int | Bar of bool

let f x = Foo x
`,
      "file:///test.ml",
    );
    let start = Types.Position.create(2, 6);
    let end = Types.Position.create(2, 7);
    let actions = await codeAction("file:///test.ml", start, end);
    expect(findAnnotateAction(actions)).toMatchInlineSnapshot(`
      Object {
        "edit": Object {
          "documentChanges": Array [
            Object {
              "edits": Array [
                Object {
                  "newText": "(x : int)",
                  "range": Object {
                    "end": Object {
                      "character": 7,
                      "line": 2,
                    },
                    "start": Object {
                      "character": 6,
                      "line": 2,
                    },
                  },
                },
              ],
              "textDocument": Object {
                "uri": "file:///test.ml",
                "version": 0,
              },
            },
          ],
        },
        "isPreferred": false,
        "kind": "type-annotate",
        "title": "Type-annotate",
      }
    `);
  });

  it("can type-annotate a toplevel value", async () => {
    await openDocument(
      outdent`
let iiii = 3 + 4
`,
      "file:///test.ml",
    );
    let start = Types.Position.create(0, 4);
    let end = Types.Position.create(0, 5);
    let actions = await codeAction("file:///test.ml", start, end);
    expect(findAnnotateAction(actions)).toMatchInlineSnapshot(`
      Object {
        "edit": Object {
          "documentChanges": Array [
            Object {
              "edits": Array [
                Object {
                  "newText": "(iiii : int)",
                  "range": Object {
                    "end": Object {
                      "character": 8,
                      "line": 0,
                    },
                    "start": Object {
                      "character": 4,
                      "line": 0,
                    },
                  },
                },
              ],
              "textDocument": Object {
                "uri": "file:///test.ml",
                "version": 0,
              },
            },
          ],
        },
        "isPreferred": false,
        "kind": "type-annotate",
        "title": "Type-annotate",
      }
    `);
  });

  it("can type-annotate an argument in a function call", async () => {
    await openDocument(
      outdent`
let f x = x + 1
let () =
  let i = 8 in
  print_int (f i)
`,
      "file:///test.ml",
    );
    let start = Types.Position.create(3, 15);
    let end = Types.Position.create(3, 16);
    let actions = await codeAction("file:///test.ml", start, end);
    expect(findAnnotateAction(actions)).toMatchInlineSnapshot(`
      Object {
        "edit": Object {
          "documentChanges": Array [
            Object {
              "edits": Array [
                Object {
                  "newText": "(i : int)",
                  "range": Object {
                    "end": Object {
                      "character": 16,
                      "line": 3,
                    },
                    "start": Object {
                      "character": 15,
                      "line": 3,
                    },
                  },
                },
              ],
              "textDocument": Object {
                "uri": "file:///test.ml",
                "version": 0,
              },
            },
          ],
        },
        "isPreferred": false,
        "kind": "type-annotate",
        "title": "Type-annotate",
      }
    `);
  });

  it("can type-annotate a variant with its name only", async () => {
    await openDocument(
      outdent`
type t = Foo of int | Bar of bool

let f (x : t) = x
`,
      "file:///test.ml",
    );
    let start = Types.Position.create(2, 16);
    let end = Types.Position.create(2, 17);
    let actions = await codeAction("file:///test.ml", start, end);
    expect(findAnnotateAction(actions)).toMatchInlineSnapshot(`
      Object {
        "edit": Object {
          "documentChanges": Array [
            Object {
              "edits": Array [
                Object {
                  "newText": "(x : t)",
                  "range": Object {
                    "end": Object {
                      "character": 17,
                      "line": 2,
                    },
                    "start": Object {
                      "character": 16,
                      "line": 2,
                    },
                  },
                },
              ],
              "textDocument": Object {
                "uri": "file:///test.ml",
                "version": 0,
              },
            },
          ],
        },
        "isPreferred": false,
        "kind": "type-annotate",
        "title": "Type-annotate",
      }
    `);
  });

  it("does not type-annotate in a non expression context", async () => {
    await openDocument(
      outdent`
type x =
   | Foo of int
   | Baz of string
`,
      "file:///test.ml",
    );
    let start = Types.Position.create(2, 5);
    let end = Types.Position.create(2, 6);
    let actions = await codeAction("file:///test.ml", start, end);
    expect(actions).toBeNull();
  });
});
