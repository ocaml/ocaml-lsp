import * as path from "node:path";
import outdent from "outdent";
import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";
import { Position } from "vscode-languageserver-types";
import { URI } from "vscode-uri";
import * as LanguageServer from "../src/LanguageServer";

function findAddRecAnnotation(actions: Types.CodeAction[]) {
  return actions.find(
    (action) =>
      action.kind === "quickfix" &&
      action.title === "Add missing `rec` keyword",
  );
}

function findMarkUnused(actions: Types.CodeAction[]) {
  return actions.find(
    (action) => action.kind === "quickfix" && action.title === "Mark as unused",
  );
}

function findRemoveUnused(actions: Types.CodeAction[]) {
  return actions.find(
    (action) => action.kind === "quickfix" && action.title === "Remove unused",
  );
}

function findInferredAction(actions: Types.CodeAction[]) {
  return actions.find((action) => action.kind === "inferred_intf");
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
      capabilities: {
        experimental: { jumpToNextHole: true },
        window: {
          showDocument: { support: true },
        },
      },
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
    if (typeof context === "undefined") {
      context = { diagnostics: [] };
    }
    return languageServer.sendRequest("textDocument/codeAction", {
      textDocument: Types.TextDocumentIdentifier.create(uri),
      context: context,
      range: { start, end },
    });
  }

  it("opens the implementation if not in store", async () => {
    const testWorkspacePath = path.join(__dirname, "declaration_files/");
    const intfFilepath = path.join(testWorkspacePath, "lib.mli");
    const intfUri = URI.file(intfFilepath).toString();
    openDocument("", intfUri);
    const start = Types.Position.create(0, 0);
    const end = Types.Position.create(0, 0);
    const actions = (await codeAction(intfUri, start, end)) ?? [];
    expect(
      findInferredAction(actions)?.edit?.documentChanges?.map((a) =>
        Types.TextDocumentEdit.is(a) ? a.edits : null,
      ),
    ).toMatchInlineSnapshot(`
[
  [
    {
      "newText": "val x : int
",
      "range": {
        "end": {
          "character": 0,
          "line": 0,
        },
        "start": {
          "character": 0,
          "line": 0,
        },
      },
    },
  ],
]
`);
  });

  it("offers `Construct an expression` code action", async () => {
    const uri = "file:///test.ml";
    openDocument(
      outdent`
let x = _
`,
      uri,
    );

    const actions =
      (await codeAction(uri, Position.create(0, 8), Position.create(0, 9))) ??
      [];

    expect(actions).toMatchInlineSnapshot(`
[
  {
    "edit": {
      "documentChanges": [
        {
          "edits": [
            {
              "newText": "(_ : 'a)",
              "range": {
                "end": {
                  "character": 9,
                  "line": 0,
                },
                "start": {
                  "character": 8,
                  "line": 0,
                },
              },
            },
          ],
          "textDocument": {
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
  {
    "command": {
      "command": "editor.action.triggerSuggest",
      "title": "Trigger Suggest",
    },
    "kind": "construct",
    "title": "Construct an expression",
  },
  {
    "edit": {
      "documentChanges": [
        {
          "edits": [
            {
              "newText": "let var_name = _ in
",
              "range": {
                "end": {
                  "character": 8,
                  "line": 0,
                },
                "start": {
                  "character": 8,
                  "line": 0,
                },
              },
            },
            {
              "newText": "var_name",
              "range": {
                "end": {
                  "character": 9,
                  "line": 0,
                },
                "start": {
                  "character": 8,
                  "line": 0,
                },
              },
            },
          ],
          "textDocument": {
            "uri": "file:///test.ml",
            "version": 0,
          },
        },
      ],
    },
    "isPreferred": false,
    "kind": "refactor.extract",
    "title": "Extract local",
  },
  {
    "edit": {
      "documentChanges": [
        {
          "edits": [
            {
              "newText": "let fun_name () = _

",
              "range": {
                "end": {
                  "character": 0,
                  "line": 0,
                },
                "start": {
                  "character": 0,
                  "line": 0,
                },
              },
            },
            {
              "newText": "fun_name ()",
              "range": {
                "end": {
                  "character": 9,
                  "line": 0,
                },
                "start": {
                  "character": 8,
                  "line": 0,
                },
              },
            },
          ],
          "textDocument": {
            "uri": "file:///test.ml",
            "version": 0,
          },
        },
      ],
    },
    "isPreferred": false,
    "kind": "refactor.extract",
    "title": "Extract function",
  },
  {
    "command": {
      "arguments": [
        "file:///test.mli",
      ],
      "command": "ocamllsp/open-related-source",
      "title": "Create test.mli",
    },
    "edit": {
      "documentChanges": [
        {
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

    const construct_actions = actions.find(
      (codeAction: Types.CodeAction) =>
        codeAction.kind && codeAction.kind === "construct",
    );

    expect(construct_actions).toMatchInlineSnapshot(`
{
  "command": {
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

    const codeActions =
      (await codeAction(documentUri, queryStartPos, queryEndPos)) ?? [];

    const specificCodeActions = codeActions.filter(
      (codeAction: Types.CodeAction) => codeAction.title === codeActionTitle,
    );

    return specificCodeActions;
  }

  it("refactor-open unqualify in-file module", async () => {
    const specificCodeActions = await testRefactorOpen({
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
[
  {
    "edit": {
      "changes": {
        "file:///test.ml": [
          {
            "newText": "f",
            "range": {
              "end": {
                "character": 11,
                "line": 7,
              },
              "start": {
                "character": 8,
                "line": 7,
              },
            },
          },
          {
            "newText": "a",
            "range": {
              "end": {
                "character": 15,
                "line": 7,
              },
              "start": {
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
    const specificCodeActions = await testRefactorOpen({
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
[
  {
    "edit": {
      "changes": {
        "file:///test.ml": [
          {
            "newText": "M.f",
            "range": {
              "end": {
                "character": 9,
                "line": 7,
              },
              "start": {
                "character": 8,
                "line": 7,
              },
            },
          },
          {
            "newText": "M.a",
            "range": {
              "end": {
                "character": 11,
                "line": 7,
              },
              "start": {
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
    const uri = "file:///missing-rec-1.ml";
    openDocument(
      outdent`
let needs_rec x = 1 + (needs_rec x)
`,
      uri,
    );
    const start = Types.Position.create(0, 31);
    const end = Types.Position.create(0, 32);
    const context = {
      diagnostics: [
        mkUnboundDiagnostic(
          Types.Position.create(0, 23),
          Types.Position.create(0, 32),
        ),
      ],
    };

    const actions = (await codeAction(uri, start, end, context)) ?? [];
    expect(findAddRecAnnotation(actions)).toMatchInlineSnapshot(`
{
  "diagnostics": [
    {
      "message": "Unbound value",
      "range": {
        "end": {
          "character": 32,
          "line": 0,
        },
        "start": {
          "character": 23,
          "line": 0,
        },
      },
      "severity": 1,
      "source": "ocamllsp",
    },
  ],
  "edit": {
    "documentChanges": [
      {
        "edits": [
          {
            "newText": "rec ",
            "range": {
              "end": {
                "character": 4,
                "line": 0,
              },
              "start": {
                "character": 4,
                "line": 0,
              },
            },
          },
        ],
        "textDocument": {
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
    const uri = "file:///missing-rec-2.ml";
    openDocument(
      outdent`
let outer =
  let inner x =
    1 + (inner
`,
      uri,
    );
    const start = Types.Position.create(2, 14);
    const end = Types.Position.create(2, 15);
    const context = {
      diagnostics: [
        mkUnboundDiagnostic(
          Types.Position.create(2, 9),
          Types.Position.create(2, 14),
        ),
      ],
    };

    const actions = (await codeAction(uri, start, end, context)) ?? [];
    expect(findAddRecAnnotation(actions)).toMatchInlineSnapshot(`
{
  "diagnostics": [
    {
      "message": "Unbound value",
      "range": {
        "end": {
          "character": 14,
          "line": 2,
        },
        "start": {
          "character": 9,
          "line": 2,
        },
      },
      "severity": 1,
      "source": "ocamllsp",
    },
  ],
  "edit": {
    "documentChanges": [
      {
        "edits": [
          {
            "newText": "rec ",
            "range": {
              "end": {
                "character": 6,
                "line": 1,
              },
              "start": {
                "character": 6,
                "line": 1,
              },
            },
          },
        ],
        "textDocument": {
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
    const uri = "file:///missing-rec-3.ml";
    openDocument(
      outdent`
let outer =
  let inner1 = 0
  and inner x =
    1 + (inner
`,
      uri,
    );
    const start = Types.Position.create(3, 14);
    const end = Types.Position.create(3, 15);
    const context = {
      diagnostics: [
        mkUnboundDiagnostic(
          Types.Position.create(3, 9),
          Types.Position.create(3, 14),
        ),
      ],
    };

    const actions = (await codeAction(uri, start, end, context)) ?? [];
    expect(findAddRecAnnotation(actions)).toMatchInlineSnapshot(`
{
  "diagnostics": [
    {
      "message": "Unbound value",
      "range": {
        "end": {
          "character": 14,
          "line": 3,
        },
        "start": {
          "character": 9,
          "line": 3,
        },
      },
      "severity": 1,
      "source": "ocamllsp",
    },
  ],
  "edit": {
    "documentChanges": [
      {
        "edits": [
          {
            "newText": "rec ",
            "range": {
              "end": {
                "character": 6,
                "line": 1,
              },
              "start": {
                "character": 6,
                "line": 1,
              },
            },
          },
        ],
        "textDocument": {
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
    const uri = "file:///has-rec-2.ml";
    openDocument(
      outdent`
let outer =
  let rec inner x =
    1 + (inner
`,
      uri,
    );
    const start = Types.Position.create(2, 14);
    const end = Types.Position.create(2, 15);

    const actions = (await codeAction(uri, start, end)) ?? [];
    expect(findAddRecAnnotation(actions)).toBeUndefined();
  });

  it("don't add rec to pattern bindings", async () => {
    const uri = "file:///no-rec-1.ml";
    openDocument(
      outdent`
let (f, x) = 1 + (f x)
`,
      uri,
    );
    const start = Types.Position.create(0, 18);
    const end = Types.Position.create(0, 19);
    const context = {
      diagnostics: [
        mkUnboundDiagnostic(
          Types.Position.create(0, 18),
          Types.Position.create(0, 19),
        ),
      ],
    };

    const actions = (await codeAction(uri, start, end, context)) ?? [];
    expect(findAddRecAnnotation(actions)).toBeUndefined();
  });

  it("mark variable as unused", async () => {
    const uri = "file:///mark-unused-variable.ml";
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
    const start = Types.Position.create(1, 6);
    const end = Types.Position.create(1, 7);
    const context = {
      diagnostics: [
        mkUnusedDiagnostic(
          Types.Position.create(1, 6),
          Types.Position.create(1, 7),
        ),
      ],
    };

    const actions = (await codeAction(uri, start, end, context)) ?? [];
    expect(findMarkUnused(actions)).toMatchInlineSnapshot(`
{
  "diagnostics": [
    {
      "message": "Error (warning 26): unused variable",
      "range": {
        "end": {
          "character": 7,
          "line": 1,
        },
        "start": {
          "character": 6,
          "line": 1,
        },
      },
      "severity": 2,
      "source": "ocamllsp",
    },
  ],
  "edit": {
    "documentChanges": [
      {
        "edits": [
          {
            "newText": "_",
            "range": {
              "end": {
                "character": 6,
                "line": 1,
              },
              "start": {
                "character": 6,
                "line": 1,
              },
            },
          },
        ],
        "textDocument": {
          "uri": "file:///mark-unused-variable.ml",
          "version": 0,
        },
      },
    ],
  },
  "isPreferred": true,
  "kind": "quickfix",
  "title": "Mark as unused",
}
`);
  });

  it("remove unused variable", async () => {
    const uri = "file:///remove-unused-variable.ml";
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
    const start = Types.Position.create(1, 6);
    const end = Types.Position.create(1, 7);
    const context = {
      diagnostics: [
        mkUnusedDiagnostic(
          Types.Position.create(1, 6),
          Types.Position.create(1, 7),
        ),
      ],
    };

    const actions = (await codeAction(uri, start, end, context)) ?? [];
    expect(findRemoveUnused(actions)).toMatchInlineSnapshot(`
{
  "diagnostics": [
    {
      "message": "Error (warning 26): unused variable",
      "range": {
        "end": {
          "character": 7,
          "line": 1,
        },
        "start": {
          "character": 6,
          "line": 1,
        },
      },
      "severity": 2,
      "source": "ocamllsp",
    },
  ],
  "edit": {
    "documentChanges": [
      {
        "edits": [
          {
            "newText": "",
            "range": {
              "end": {
                "character": 2,
                "line": 5,
              },
              "start": {
                "character": 2,
                "line": 1,
              },
            },
          },
        ],
        "textDocument": {
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
    const uri = "file:///remove-unused-variable-2.ml";
    openDocument(
      outdent`
let f x =
  let y = 0 and z = 0 in
  0
`,
      uri,
    );
    const start = Types.Position.create(1, 6);
    const end = Types.Position.create(1, 7);
    const context = {
      diagnostics: [
        mkUnusedDiagnostic(
          Types.Position.create(1, 6),
          Types.Position.create(1, 7),
        ),
      ],
    };

    const actions = (await codeAction(uri, start, end, context)) ?? [];
    expect(findRemoveUnused(actions)).toBeUndefined();
  });
});
