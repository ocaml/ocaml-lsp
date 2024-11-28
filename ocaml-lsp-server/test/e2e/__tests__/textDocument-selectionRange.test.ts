import outdent from "outdent";
import * as LanguageServer from "../src/LanguageServer";
import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";

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

    let result = await selectionRange([Types.Position.create(1, 17)]);
    expect(result).toMatchInlineSnapshot(`
Array [
  Object {
    "parent": Object {
      "parent": Object {
        "parent": Object {
          "parent": Object {
            "parent": Object {
              "parent": Object {
                "range": Object {
                  "end": Object {
                    "character": 17,
                    "line": 3,
                  },
                  "start": Object {
                    "character": 0,
                    "line": 0,
                  },
                },
              },
              "range": Object {
                "end": Object {
                  "character": 17,
                  "line": 3,
                },
                "start": Object {
                  "character": 8,
                  "line": 0,
                },
              },
            },
            "range": Object {
              "end": Object {
                "character": 17,
                "line": 3,
              },
              "start": Object {
                "character": 10,
                "line": 0,
              },
            },
          },
          "range": Object {
            "end": Object {
              "character": 17,
              "line": 3,
            },
            "start": Object {
              "character": 2,
              "line": 1,
            },
          },
        },
        "range": Object {
          "end": Object {
            "character": 22,
            "line": 1,
          },
          "start": Object {
            "character": 2,
            "line": 1,
          },
        },
      },
      "range": Object {
        "end": Object {
          "character": 22,
          "line": 1,
        },
        "start": Object {
          "character": 15,
          "line": 1,
        },
      },
    },
    "range": Object {
      "end": Object {
        "character": 18,
        "line": 1,
      },
      "start": Object {
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

    let result = await selectionRange([Types.Position.create(5, 23)]);
    expect(result).toMatchInlineSnapshot(`
Array [
  Object {
    "parent": Object {
      "parent": Object {
        "parent": Object {
          "parent": Object {
            "parent": Object {
              "parent": Object {
                "parent": Object {
                  "range": Object {
                    "end": Object {
                      "character": 3,
                      "line": 6,
                    },
                    "start": Object {
                      "character": 0,
                      "line": 0,
                    },
                  },
                },
                "range": Object {
                  "end": Object {
                    "character": 3,
                    "line": 6,
                  },
                  "start": Object {
                    "character": 0,
                    "line": 3,
                  },
                },
              },
              "range": Object {
                "end": Object {
                  "character": 3,
                  "line": 6,
                },
                "start": Object {
                  "character": 11,
                  "line": 3,
                },
              },
            },
            "range": Object {
              "end": Object {
                "character": 24,
                "line": 5,
              },
              "start": Object {
                "character": 2,
                "line": 4,
              },
            },
          },
          "range": Object {
            "end": Object {
              "character": 24,
              "line": 5,
            },
            "start": Object {
              "character": 2,
              "line": 5,
            },
          },
        },
        "range": Object {
          "end": Object {
            "character": 24,
            "line": 5,
          },
          "start": Object {
            "character": 8,
            "line": 5,
          },
        },
      },
      "range": Object {
        "end": Object {
          "character": 24,
          "line": 5,
        },
        "start": Object {
          "character": 9,
          "line": 5,
        },
      },
    },
    "range": Object {
      "end": Object {
        "character": 24,
        "line": 5,
      },
      "start": Object {
        "character": 22,
        "line": 5,
      },
    },
  },
]
`)});

  it("returns a selection range for functors", async () => {
    openDocument(outdent`
module M = Map.Make (struct
  type t = { o: < rank : int > }
  let compare a b = a.o#rank - b.o#rank
end)`);

  let result = await selectionRange([Types.Position.create(2, 26)]);
  expect(result).toMatchInlineSnapshot(`
Array [
  Object {
    "parent": Object {
      "parent": Object {
        "parent": Object {
          "parent": Object {
            "parent": Object {
              "parent": Object {
                "parent": Object {
                  "parent": Object {
                    "parent": Object {
                      "range": Object {
                        "end": Object {
                          "character": 4,
                          "line": 3,
                        },
                        "start": Object {
                          "character": 0,
                          "line": 0,
                        },
                      },
                    },
                    "range": Object {
                      "end": Object {
                        "character": 4,
                        "line": 3,
                      },
                      "start": Object {
                        "character": 11,
                        "line": 0,
                      },
                    },
                  },
                  "range": Object {
                    "end": Object {
                      "character": 3,
                      "line": 3,
                    },
                    "start": Object {
                      "character": 21,
                      "line": 0,
                    },
                  },
                },
                "range": Object {
                  "end": Object {
                    "character": 39,
                    "line": 2,
                  },
                  "start": Object {
                    "character": 2,
                    "line": 1,
                  },
                },
              },
              "range": Object {
                "end": Object {
                  "character": 39,
                  "line": 2,
                },
                "start": Object {
                  "character": 2,
                  "line": 2,
                },
              },
            },
            "range": Object {
              "end": Object {
                "character": 39,
                "line": 2,
              },
              "start": Object {
                "character": 14,
                "line": 2,
              },
            },
          },
          "range": Object {
            "end": Object {
              "character": 39,
              "line": 2,
            },
            "start": Object {
              "character": 16,
              "line": 2,
            },
          },
        },
        "range": Object {
          "end": Object {
            "character": 39,
            "line": 2,
          },
          "start": Object {
            "character": 20,
            "line": 2,
          },
        },
      },
      "range": Object {
        "end": Object {
          "character": 28,
          "line": 2,
        },
        "start": Object {
          "character": 20,
          "line": 2,
        },
      },
    },
    "range": Object {
      "end": Object {
        "character": 28,
        "line": 2,
      },
      "start": Object {
        "character": 23,
        "line": 2,
      },
    },
  },
]
`)});

it("returns a reasonable selection range for ill-typed modules", async () => {
  openDocument(outdent`
module M = struct
    let f x : int = string_of_int x
end`);

let result = await selectionRange([Types.Position.create(1, 34)]);
expect(result).toMatchInlineSnapshot(`
Array [
  Object {
    "parent": Object {
      "parent": Object {
        "parent": Object {
          "parent": Object {
            "parent": Object {
              "range": Object {
                "end": Object {
                  "character": 3,
                  "line": 2,
                },
                "start": Object {
                  "character": 0,
                  "line": 0,
                },
              },
            },
            "range": Object {
              "end": Object {
                "character": 3,
                "line": 2,
              },
              "start": Object {
                "character": 11,
                "line": 0,
              },
            },
          },
          "range": Object {
            "end": Object {
              "character": 35,
              "line": 1,
            },
            "start": Object {
              "character": 4,
              "line": 1,
            },
          },
        },
        "range": Object {
          "end": Object {
            "character": 35,
            "line": 1,
          },
          "start": Object {
            "character": 10,
            "line": 1,
          },
        },
      },
      "range": Object {
        "end": Object {
          "character": 35,
          "line": 1,
        },
        "start": Object {
          "character": 20,
          "line": 1,
        },
      },
    },
    "range": Object {
      "end": Object {
        "character": 35,
        "line": 1,
      },
      "start": Object {
        "character": 34,
        "line": 1,
      },
    },
  },
]
`)});

it("returns a reasonable selection range in the presence of syntax errors", async () => {
  openDocument(outdent`
module M = struct
    let f x : int = string_of_int x
ed`);

let result = await selectionRange([Types.Position.create(1, 34)]);
expect(result).toMatchInlineSnapshot(`
Array [
  Object {
    "parent": Object {
      "parent": Object {
        "parent": Object {
          "parent": Object {
            "parent": Object {
              "range": Object {
                "end": Object {
                  "character": 2,
                  "line": 2,
                },
                "start": Object {
                  "character": 0,
                  "line": 0,
                },
              },
            },
            "range": Object {
              "end": Object {
                "character": 2,
                "line": 2,
              },
              "start": Object {
                "character": 11,
                "line": 0,
              },
            },
          },
          "range": Object {
            "end": Object {
              "character": 2,
              "line": 2,
            },
            "start": Object {
              "character": 4,
              "line": 1,
            },
          },
        },
        "range": Object {
          "end": Object {
            "character": 2,
            "line": 2,
          },
          "start": Object {
            "character": 10,
            "line": 1,
          },
        },
      },
      "range": Object {
        "end": Object {
          "character": 2,
          "line": 2,
        },
        "start": Object {
          "character": 20,
          "line": 1,
        },
      },
    },
    "range": Object {
      "end": Object {
        "character": 35,
        "line": 1,
      },
      "start": Object {
        "character": 34,
        "line": 1,
      },
    },
  },
]
`)});
});
