import outdent from "outdent";
import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";
import * as LanguageServer from "../src/LanguageServer";

describe("textDocument/selectionRange", () => {
  let languageServer: LanguageServer.LanguageServer;

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
  });

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

  async function selectionRange(positions: Types.Position[]) {
    return await languageServer.sendRequest(
      Protocol.SelectionRangeRequest.type,
      {
        textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
        positions: positions,
      },
    );
  }

  it("returns a selection range for modules", async () => {
    openDocument(outdent`
      let foo a b =
        let min_ab = min a b in
        let max_ab = max a b in
        min_ab * max_ab
        `);

    const result = await selectionRange([Types.Position.create(1, 17)]);
    expect(result).toMatchInlineSnapshot(`
[
  {
    "parent": {
      "parent": {
        "parent": {
          "parent": {
            "parent": {
              "range": {
                "end": {
                  "character": 17,
                  "line": 3,
                },
                "start": {
                  "character": 0,
                  "line": 0,
                },
              },
            },
            "range": {
              "end": {
                "character": 17,
                "line": 3,
              },
              "start": {
                "character": 8,
                "line": 0,
              },
            },
          },
          "range": {
            "end": {
              "character": 17,
              "line": 3,
            },
            "start": {
              "character": 2,
              "line": 1,
            },
          },
        },
        "range": {
          "end": {
            "character": 22,
            "line": 1,
          },
          "start": {
            "character": 2,
            "line": 1,
          },
        },
      },
      "range": {
        "end": {
          "character": 22,
          "line": 1,
        },
        "start": {
          "character": 15,
          "line": 1,
        },
      },
    },
    "range": {
      "end": {
        "character": 18,
        "line": 1,
      },
      "start": {
        "character": 15,
        "line": 1,
      },
    },
  },
]
`);
  });

  it("returns a selection range for more complex documents", async () => {
    openDocument(outdent`
      type _ typ =
        | TInt : int typ
        | TBool : bool typ
      module M = struct
        type t
        let f (_ : _ typ) = ()
      end
        `);

    const result = await selectionRange([Types.Position.create(5, 23)]);
    expect(result).toMatchInlineSnapshot(`
[
  {
    "parent": {
      "parent": {
        "parent": {
          "parent": {
            "parent": {
              "parent": {
                "range": {
                  "end": {
                    "character": 3,
                    "line": 6,
                  },
                  "start": {
                    "character": 0,
                    "line": 0,
                  },
                },
              },
              "range": {
                "end": {
                  "character": 3,
                  "line": 6,
                },
                "start": {
                  "character": 0,
                  "line": 3,
                },
              },
            },
            "range": {
              "end": {
                "character": 3,
                "line": 6,
              },
              "start": {
                "character": 11,
                "line": 3,
              },
            },
          },
          "range": {
            "end": {
              "character": 24,
              "line": 5,
            },
            "start": {
              "character": 2,
              "line": 4,
            },
          },
        },
        "range": {
          "end": {
            "character": 24,
            "line": 5,
          },
          "start": {
            "character": 2,
            "line": 5,
          },
        },
      },
      "range": {
        "end": {
          "character": 24,
          "line": 5,
        },
        "start": {
          "character": 8,
          "line": 5,
        },
      },
    },
    "range": {
      "end": {
        "character": 24,
        "line": 5,
      },
      "start": {
        "character": 22,
        "line": 5,
      },
    },
  },
]
`);
  });

  it("returns a selection range for functors", async () => {
    openDocument(outdent`
module M = Map.Make (struct
  type t = { o: < rank : int > }
  let compare a b = a.o#rank - b.o#rank
end)`);

    const result = await selectionRange([Types.Position.create(2, 26)]);
    expect(result).toMatchInlineSnapshot(`
[
  {
    "parent": {
      "parent": {
        "parent": {
          "parent": {
            "parent": {
              "parent": {
                "parent": {
                  "parent": {
                    "range": {
                      "end": {
                        "character": 4,
                        "line": 3,
                      },
                      "start": {
                        "character": 0,
                        "line": 0,
                      },
                    },
                  },
                  "range": {
                    "end": {
                      "character": 4,
                      "line": 3,
                    },
                    "start": {
                      "character": 11,
                      "line": 0,
                    },
                  },
                },
                "range": {
                  "end": {
                    "character": 3,
                    "line": 3,
                  },
                  "start": {
                    "character": 21,
                    "line": 0,
                  },
                },
              },
              "range": {
                "end": {
                  "character": 39,
                  "line": 2,
                },
                "start": {
                  "character": 2,
                  "line": 1,
                },
              },
            },
            "range": {
              "end": {
                "character": 39,
                "line": 2,
              },
              "start": {
                "character": 2,
                "line": 2,
              },
            },
          },
          "range": {
            "end": {
              "character": 39,
              "line": 2,
            },
            "start": {
              "character": 14,
              "line": 2,
            },
          },
        },
        "range": {
          "end": {
            "character": 39,
            "line": 2,
          },
          "start": {
            "character": 20,
            "line": 2,
          },
        },
      },
      "range": {
        "end": {
          "character": 28,
          "line": 2,
        },
        "start": {
          "character": 20,
          "line": 2,
        },
      },
    },
    "range": {
      "end": {
        "character": 28,
        "line": 2,
      },
      "start": {
        "character": 23,
        "line": 2,
      },
    },
  },
]
`);
  });

  it("returns a reasonable selection range for ill-typed modules", async () => {
    openDocument(outdent`
module M = struct
    let f x : int = string_of_int x
end`);

    const result = await selectionRange([Types.Position.create(1, 34)]);
    expect(result).toMatchInlineSnapshot(`
[
  {
    "parent": {
      "parent": {
        "parent": {
          "parent": {
            "parent": {
              "range": {
                "end": {
                  "character": 3,
                  "line": 2,
                },
                "start": {
                  "character": 0,
                  "line": 0,
                },
              },
            },
            "range": {
              "end": {
                "character": 3,
                "line": 2,
              },
              "start": {
                "character": 11,
                "line": 0,
              },
            },
          },
          "range": {
            "end": {
              "character": 35,
              "line": 1,
            },
            "start": {
              "character": 4,
              "line": 1,
            },
          },
        },
        "range": {
          "end": {
            "character": 35,
            "line": 1,
          },
          "start": {
            "character": 10,
            "line": 1,
          },
        },
      },
      "range": {
        "end": {
          "character": 35,
          "line": 1,
        },
        "start": {
          "character": 20,
          "line": 1,
        },
      },
    },
    "range": {
      "end": {
        "character": 35,
        "line": 1,
      },
      "start": {
        "character": 34,
        "line": 1,
      },
    },
  },
]
`);
  });

  it("returns a reasonable selection range in the presence of syntax errors", async () => {
    openDocument(outdent`
module M = struct
    let f x : int = string_of_int x
ed`);

    const result = await selectionRange([Types.Position.create(1, 34)]);
    expect(result).toMatchInlineSnapshot(`
[
  {
    "parent": {
      "parent": {
        "parent": {
          "parent": {
            "parent": {
              "range": {
                "end": {
                  "character": 2,
                  "line": 2,
                },
                "start": {
                  "character": 0,
                  "line": 0,
                },
              },
            },
            "range": {
              "end": {
                "character": 2,
                "line": 2,
              },
              "start": {
                "character": 11,
                "line": 0,
              },
            },
          },
          "range": {
            "end": {
              "character": 2,
              "line": 2,
            },
            "start": {
              "character": 4,
              "line": 1,
            },
          },
        },
        "range": {
          "end": {
            "character": 2,
            "line": 2,
          },
          "start": {
            "character": 10,
            "line": 1,
          },
        },
      },
      "range": {
        "end": {
          "character": 2,
          "line": 2,
        },
        "start": {
          "character": 20,
          "line": 1,
        },
      },
    },
    "range": {
      "end": {
        "character": 35,
        "line": 1,
      },
      "start": {
        "character": 34,
        "line": 1,
      },
    },
  },
]
`);
  });
});
