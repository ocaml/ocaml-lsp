import outdent from "outdent";
import * as path from "path";
import { URI } from "vscode-uri";
import * as LanguageServer from "../src/LanguageServer";
import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";
import { Position } from "vscode-languageserver-types";

function findAnnotateAction(actions: Types.CodeAction[]) {
  return actions.find((action) => action.kind == "type-annotate");
}

function findAddRecAnnotation(actions: Types.CodeAction[]) {
  return actions.find(
    (action) =>
      action.kind == "quickfix" && action.title == "Add missing `rec` keyword",
  );
}

function findMarkUnused(actions: Types.CodeAction[]) {
  return actions.find(
    (action) => action.kind == "quickfix" && action.title == "Mark as unused",
  );
}

function findRemoveUnused(actions: Types.CodeAction[]) {
  return actions.find(
    (action) => action.kind == "quickfix" && action.title == "Remove unused",
  );
}

function findInferredAction(actions: Types.CodeAction[]) {
  return actions.find((action) => action.kind == "inferred_intf");
}

function mkUnboundDiagnostic(start: Types.Position, end: Types.Position) {
  return {
    message: "Unbound value",
    range: { end, start },
    severity: Types.DiagnosticSeverity.Error,
    source: "ocamllsp",
  };
}

function mkUnusedDiagnostic(start: Types.Position, end: Types.Position) {
  return {
    message: "Error (warning 26): unused variable",
    range: { end, start },
    severity: Types.DiagnosticSeverity.Warning,
    source: "ocamllsp",
  };
}

describe("textDocument/codeAction", () => {
  let languageServer: LanguageServer.LanguageServer;

  function openDocument(source: string, uri: string) {
    languageServer.sendNotification(
      Protocol.DidOpenTextDocumentNotification.type,
      {
        textDocument: Types.TextDocumentItem.create(uri, "ocaml", 0, source),
      },
    );
  }

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize({
      capabilities: { experimental: { jumpToNextHole: true } },
    });
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
  });

  async function codeAction(
    uri: string,
    start: Position,
    end: Position,
    context?: Types.CodeActionContext,
  ): Promise<Array<Types.CodeAction> | null> {
    if (typeof context == "undefined") {
      context = { diagnostics: [] };
    }
    return languageServer.sendRequest("textDocument/codeAction", {
      textDocument: Types.TextDocumentIdentifier.create(uri),
      context: context,
      range: { start, end },
    });
  }

  it("can destruct sum types", async () => {
    openDocument(
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
                "inRange": Object {
                  "end": Object {
                    "character": 54,
                    "line": 2,
                  },
                  "start": Object {
                    "character": 16,
                    "line": 2,
                  },
                },
                "shouldNotifyIfNoHole": false,
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
        Object {
          "command": Object {
            "arguments": Array [
              "file:///test.mli",
            ],
            "command": "ocamllsp/open-related-source",
            "title": "Create test.mli",
          },
          "edit": Object {
            "documentChanges": Array [
              Object {
                "kind": "create",
                "uri": "file:///test.mli",
              },
            ],
          },
          "kind": "switch",
          "title": "Create test.mli",
        },
      ]
    `);
  });

  it("can infer module interfaces", async () => {
    openDocument(
      outdent`
type t = Foo of int | Bar of bool

let f (x : t) = x
`,
      "file:///test.ml",
    );
    openDocument("", "file:///test.mli");
    let start = Types.Position.create(0, 0);
    let end = Types.Position.create(0, 0);
    let actions = (await codeAction("file:///test.mli", start, end)) ?? [];
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
    let intfUri = URI.file(intfFilepath).toString();
    openDocument("", intfUri);
    let start = Types.Position.create(0, 0);
    let end = Types.Position.create(0, 0);
    let actions = (await codeAction(intfUri, start, end)) ?? [];
    expect(
      findInferredAction(actions)?.edit?.documentChanges?.map((a) =>
        Types.TextDocumentEdit.is(a) ? a.edits : null,
      ),
    ).toMatchInlineSnapshot(`
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
    openDocument(
      outdent`
type t = Foo of int | Bar of bool

let f x = Foo x
`,
      "file:///test.ml",
    );
    let start = Types.Position.create(2, 6);
    let end = Types.Position.create(2, 7);
    let actions = (await codeAction("file:///test.ml", start, end)) ?? [];
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
    openDocument(
      outdent`
let iiii = 3 + 4
`,
      "file:///test.ml",
    );
    let start = Types.Position.create(0, 4);
    let end = Types.Position.create(0, 5);
    let actions = (await codeAction("file:///test.ml", start, end)) ?? [];
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
    openDocument(
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
    let actions = (await codeAction("file:///test.ml", start, end)) ?? [];
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
    openDocument(
      outdent`
type t = Foo of int | Bar of bool

let f (x : t) = x
`,
      "file:///test.ml",
    );
    let start = Types.Position.create(2, 16);
    let end = Types.Position.create(2, 17);
    let actions = (await codeAction("file:///test.ml", start, end)) ?? [];
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
    openDocument(
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
    expect(actions).toMatchInlineSnapshot(`
      Array [
        Object {
          "command": Object {
            "arguments": Array [
              "file:///test.mli",
            ],
            "command": "ocamllsp/open-related-source",
            "title": "Create test.mli",
          },
          "edit": Object {
            "documentChanges": Array [
              Object {
                "kind": "create",
                "uri": "file:///test.mli",
              },
            ],
          },
          "kind": "switch",
          "title": "Create test.mli",
        },
      ]
    `);
  });

  it("offers `Construct an expression` code action", async () => {
    let uri = "file:///test.ml";
    openDocument(
      outdent`
let x = _
`,
      uri,
    );

    let actions =
      (await codeAction(uri, Position.create(0, 8), Position.create(0, 9))) ??
      [];

    expect(actions).toMatchInlineSnapshot(`
      Array [
        Object {
          "edit": Object {
            "documentChanges": Array [
              Object {
                "edits": Array [
                  Object {
                    "newText": "(_ : 'a)",
                    "range": Object {
                      "end": Object {
                        "character": 9,
                        "line": 0,
                      },
                      "start": Object {
                        "character": 8,
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
        },
        Object {
          "command": Object {
            "command": "editor.action.triggerSuggest",
            "title": "Trigger Suggest",
          },
          "kind": "construct",
          "title": "Construct an expression",
        },
        Object {
          "command": Object {
            "arguments": Array [
              "file:///test.mli",
            ],
            "command": "ocamllsp/open-related-source",
            "title": "Create test.mli",
          },
          "edit": Object {
            "documentChanges": Array [
              Object {
                "kind": "create",
                "uri": "file:///test.mli",
              },
            ],
          },
          "kind": "switch",
          "title": "Create test.mli",
        },
      ]
    `);

    let construct_actions = actions.find(
      (codeAction: Types.CodeAction) =>
        codeAction.kind && codeAction.kind === "construct",
    );

    expect(construct_actions).toMatchInlineSnapshot(`
      Object {
        "command": Object {
          "command": "editor.action.triggerSuggest",
          "title": "Trigger Suggest",
        },
        "kind": "construct",
        "title": "Construct an expression",
      }
    `);
  });

  type refactorOpenTestSpec = {
    documentUri?: string;
    documentText: string;
    queryStartPos: Types.Position;
    queryEndPos: Types.Position;
    codeActionTitle: string;
  };

  // this removes some repetition in code for testing `refactor-open` code actions
  // it specifically doesn't include `expect(...).toMatchInlineSnapshot` to be able to
  // capture correct output (the snapshot) from jest automatically
  // (similar to ppx_expect promotion with correct output)
  async function testRefactorOpen({
    documentUri,
    documentText,
    queryStartPos,
    queryEndPos,
    codeActionTitle,
  }: refactorOpenTestSpec) {
    documentUri = documentUri ? documentUri : "file:///test.ml";

    openDocument(documentText, documentUri);

    let codeActions =
      (await codeAction(documentUri, queryStartPos, queryEndPos)) ?? [];

    let specificCodeActions = codeActions.filter(
      (codeAction: Types.CodeAction) => codeAction.title === codeActionTitle,
    );

    return specificCodeActions;
  }

  it("refactor-open unqualify in-file module", async () => {
    let specificCodeActions = await testRefactorOpen({
      documentText: outdent`
      module M = struct
        let a = 1
        let f x = x + 1
      end

      open M

      let y = M.f M.a
      `,
      queryStartPos: Types.Position.create(6, 5),
      queryEndPos: Types.Position.create(6, 5),
      codeActionTitle: "Remove module name from identifiers",
    });

    expect(specificCodeActions).toMatchInlineSnapshot(`
      Array [
        Object {
          "edit": Object {
            "changes": Object {
              "file:///test.ml": Array [
                Object {
                  "newText": "f",
                  "range": Object {
                    "end": Object {
                      "character": 11,
                      "line": 7,
                    },
                    "start": Object {
                      "character": 8,
                      "line": 7,
                    },
                  },
                },
                Object {
                  "newText": "a",
                  "range": Object {
                    "end": Object {
                      "character": 15,
                      "line": 7,
                    },
                    "start": Object {
                      "character": 12,
                      "line": 7,
                    },
                  },
                },
              ],
            },
          },
          "isPreferred": false,
          "kind": "remove module name from identifiers",
          "title": "Remove module name from identifiers",
        },
      ]
    `);
  });

  it("refactor-open qualify in-file module", async () => {
    let specificCodeActions = await testRefactorOpen({
      documentText: outdent`
      module M = struct
        let a = 1
        let f x = x + 1
      end

      open M

      let y = f a
      `,
      queryStartPos: Types.Position.create(6, 5),
      queryEndPos: Types.Position.create(6, 5),
      codeActionTitle: "Put module name in identifiers",
    });

    expect(specificCodeActions).toMatchInlineSnapshot(`
      Array [
        Object {
          "edit": Object {
            "changes": Object {
              "file:///test.ml": Array [
                Object {
                  "newText": "M.f",
                  "range": Object {
                    "end": Object {
                      "character": 9,
                      "line": 7,
                    },
                    "start": Object {
                      "character": 8,
                      "line": 7,
                    },
                  },
                },
                Object {
                  "newText": "M.a",
                  "range": Object {
                    "end": Object {
                      "character": 11,
                      "line": 7,
                    },
                    "start": Object {
                      "character": 10,
                      "line": 7,
                    },
                  },
                },
              ],
            },
          },
          "isPreferred": false,
          "kind": "put module name in identifiers",
          "title": "Put module name in identifiers",
        },
      ]
    `);
  });

  it("add missing rec in toplevel let", async () => {
    let uri = "file:///missing-rec-1.ml";
    openDocument(
      outdent`
let needs_rec x = 1 + (needs_rec x)
`,
      uri,
    );
    let start = Types.Position.create(0, 31);
    let end = Types.Position.create(0, 32);
    let context = {
      diagnostics: [
        mkUnboundDiagnostic(
          Types.Position.create(0, 23),
          Types.Position.create(0, 32),
        ),
      ],
    };

    let actions = (await codeAction(uri, start, end, context)) ?? [];
    expect(findAddRecAnnotation(actions)).toMatchInlineSnapshot(`
      Object {
        "diagnostics": Array [
          Object {
            "message": "Unbound value",
            "range": Object {
              "end": Object {
                "character": 32,
                "line": 0,
              },
              "start": Object {
                "character": 23,
                "line": 0,
              },
            },
            "severity": 1,
            "source": "ocamllsp",
          },
        ],
        "edit": Object {
          "documentChanges": Array [
            Object {
              "edits": Array [
                Object {
                  "newText": "rec ",
                  "range": Object {
                    "end": Object {
                      "character": 4,
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
                "uri": "file:///missing-rec-1.ml",
                "version": 0,
              },
            },
          ],
        },
        "isPreferred": false,
        "kind": "quickfix",
        "title": "Add missing \`rec\` keyword",
      }
    `);
  });

  it("add missing rec in expression let", async () => {
    let uri = "file:///missing-rec-2.ml";
    openDocument(
      outdent`
let outer =
  let inner x =
    1 + (inner
`,
      uri,
    );
    let start = Types.Position.create(2, 14);
    let end = Types.Position.create(2, 15);
    let context = {
      diagnostics: [
        mkUnboundDiagnostic(
          Types.Position.create(2, 9),
          Types.Position.create(2, 14),
        ),
      ],
    };

    let actions = (await codeAction(uri, start, end, context)) ?? [];
    expect(findAddRecAnnotation(actions)).toMatchInlineSnapshot(`
      Object {
        "diagnostics": Array [
          Object {
            "message": "Unbound value",
            "range": Object {
              "end": Object {
                "character": 14,
                "line": 2,
              },
              "start": Object {
                "character": 9,
                "line": 2,
              },
            },
            "severity": 1,
            "source": "ocamllsp",
          },
        ],
        "edit": Object {
          "documentChanges": Array [
            Object {
              "edits": Array [
                Object {
                  "newText": "rec ",
                  "range": Object {
                    "end": Object {
                      "character": 6,
                      "line": 1,
                    },
                    "start": Object {
                      "character": 6,
                      "line": 1,
                    },
                  },
                },
              ],
              "textDocument": Object {
                "uri": "file:///missing-rec-2.ml",
                "version": 0,
              },
            },
          ],
        },
        "isPreferred": false,
        "kind": "quickfix",
        "title": "Add missing \`rec\` keyword",
      }
    `);
  });

  it("add missing rec in expression let-and", async () => {
    let uri = "file:///missing-rec-3.ml";
    openDocument(
      outdent`
let outer =
  let inner1 = 0
  and inner x =
    1 + (inner
`,
      uri,
    );
    let start = Types.Position.create(3, 14);
    let end = Types.Position.create(3, 15);
    let context = {
      diagnostics: [
        mkUnboundDiagnostic(
          Types.Position.create(3, 9),
          Types.Position.create(3, 14),
        ),
      ],
    };

    let actions = (await codeAction(uri, start, end, context)) ?? [];
    expect(findAddRecAnnotation(actions)).toMatchInlineSnapshot(`
      Object {
        "diagnostics": Array [
          Object {
            "message": "Unbound value",
            "range": Object {
              "end": Object {
                "character": 14,
                "line": 3,
              },
              "start": Object {
                "character": 9,
                "line": 3,
              },
            },
            "severity": 1,
            "source": "ocamllsp",
          },
        ],
        "edit": Object {
          "documentChanges": Array [
            Object {
              "edits": Array [
                Object {
                  "newText": "rec ",
                  "range": Object {
                    "end": Object {
                      "character": 6,
                      "line": 1,
                    },
                    "start": Object {
                      "character": 6,
                      "line": 1,
                    },
                  },
                },
              ],
              "textDocument": Object {
                "uri": "file:///missing-rec-3.ml",
                "version": 0,
              },
            },
          ],
        },
        "isPreferred": false,
        "kind": "quickfix",
        "title": "Add missing \`rec\` keyword",
      }
    `);
  });

  it("don't add rec when rec exists", async () => {
    let uri = "file:///has-rec-2.ml";
    openDocument(
      outdent`
let outer =
  let rec inner x =
    1 + (inner
`,
      uri,
    );
    let start = Types.Position.create(2, 14);
    let end = Types.Position.create(2, 15);

    let actions = (await codeAction(uri, start, end)) ?? [];
    expect(findAddRecAnnotation(actions)).toBeUndefined();
  });

  it("don't add rec to pattern bindings", async () => {
    let uri = "file:///no-rec-1.ml";
    openDocument(
      outdent`
let (f, x) = 1 + (f x)
`,
      uri,
    );
    let start = Types.Position.create(0, 18);
    let end = Types.Position.create(0, 19);
    let context = {
      diagnostics: [
        mkUnboundDiagnostic(
          Types.Position.create(0, 18),
          Types.Position.create(0, 19),
        ),
      ],
    };

    let actions = (await codeAction(uri, start, end, context)) ?? [];
    expect(findAddRecAnnotation(actions)).toBeUndefined();
  });

  it("mark variable as unused", async () => {
    let uri = "file:///mark-unused-variable.ml";
    openDocument(
      outdent`
let f x =
  let y = [
    1;
    2;
  ] in
  0
`,
      uri,
    );
    let start = Types.Position.create(1, 6);
    let end = Types.Position.create(1, 7);
    let context = {
      diagnostics: [
        mkUnusedDiagnostic(
          Types.Position.create(1, 6),
          Types.Position.create(1, 7),
        ),
      ],
    };

    let actions = (await codeAction(uri, start, end, context)) ?? [];
    expect(findMarkUnused(actions)).toMatchInlineSnapshot(`
      Object {
        "diagnostics": Array [
          Object {
            "message": "Error (warning 26): unused variable",
            "range": Object {
              "end": Object {
                "character": 7,
                "line": 1,
              },
              "start": Object {
                "character": 6,
                "line": 1,
              },
            },
            "severity": 2,
            "source": "ocamllsp",
          },
        ],
        "edit": Object {
          "documentChanges": Array [
            Object {
              "edits": Array [
                Object {
                  "newText": "_",
                  "range": Object {
                    "end": Object {
                      "character": 6,
                      "line": 1,
                    },
                    "start": Object {
                      "character": 6,
                      "line": 1,
                    },
                  },
                },
              ],
              "textDocument": Object {
                "uri": "file:///mark-unused-variable.ml",
                "version": 0,
              },
            },
          ],
        },
        "isPreferred": false,
        "kind": "quickfix",
        "title": "Mark as unused",
      }
    `);
  });

  it("remove unused variable", async () => {
    let uri = "file:///remove-unused-variable.ml";
    openDocument(
      outdent`
let f x =
  let y = [
    1;
    2;
  ] in
  0
`,
      uri,
    );
    let start = Types.Position.create(1, 6);
    let end = Types.Position.create(1, 7);
    let context = {
      diagnostics: [
        mkUnusedDiagnostic(
          Types.Position.create(1, 6),
          Types.Position.create(1, 7),
        ),
      ],
    };

    let actions = (await codeAction(uri, start, end, context)) ?? [];
    expect(findRemoveUnused(actions)).toMatchInlineSnapshot(`
      Object {
        "diagnostics": Array [
          Object {
            "message": "Error (warning 26): unused variable",
            "range": Object {
              "end": Object {
                "character": 7,
                "line": 1,
              },
              "start": Object {
                "character": 6,
                "line": 1,
              },
            },
            "severity": 2,
            "source": "ocamllsp",
          },
        ],
        "edit": Object {
          "documentChanges": Array [
            Object {
              "edits": Array [
                Object {
                  "newText": "",
                  "range": Object {
                    "end": Object {
                      "character": 2,
                      "line": 5,
                    },
                    "start": Object {
                      "character": 2,
                      "line": 1,
                    },
                  },
                },
              ],
              "textDocument": Object {
                "uri": "file:///remove-unused-variable.ml",
                "version": 0,
              },
            },
          ],
        },
        "isPreferred": false,
        "kind": "quickfix",
        "title": "Remove unused",
      }
    `);
  });

  it("don't remove unused value in let-and binding", async () => {
    let uri = "file:///remove-unused-variable-2.ml";
    openDocument(
      outdent`
let f x =
  let y = 0 and z = 0 in
  0
`,
      uri,
    );
    let start = Types.Position.create(1, 6);
    let end = Types.Position.create(1, 7);
    let context = {
      diagnostics: [
        mkUnusedDiagnostic(
          Types.Position.create(1, 6),
          Types.Position.create(1, 7),
        ),
      ],
    };

    let actions = (await codeAction(uri, start, end, context)) ?? [];
    expect(findRemoveUnused(actions)).toBeUndefined();
  });
});
