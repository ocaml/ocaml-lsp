import outdent from "outdent";
import * as LanguageServer from "./../src/LanguageServer";
import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";

const describe_opt = LanguageServer.ocamlVersionGEq("4.08.0")
  ? describe
  : xdescribe;

describe_opt("textDocument/completion", () => {
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

  async function querySignatureHelp(position: Types.Position) {
    return await languageServer.sendRequest(
      Protocol.SignatureHelpRequest.type,
      {
        textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
        position,
      },
    );
  }

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize({
      capabilities: {
        textDocument: {
          moniker: {},
          signatureHelp: {
            dynamicRegistration: true,
            signatureInformation: {
              documentationFormat: ["markdown", "plaintext"],
              parameterInformation: {
                labelOffsetSupport: true,
              },
            },
          },
        },
      },
    });
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
  });

  it("can provide signature help after a function-type value", async () => {
    openDocument(outdent`
      let map = ListLabels.map

      let _ = map
    `);

    let items = await querySignatureHelp(Types.Position.create(2, 11));
    expect(items).toMatchInlineSnapshot(`
Object {
  "activeParameter": 1,
  "activeSignature": 0,
  "signatures": Array [
    Object {
      "label": "map : f:('a -> 'b) -> 'a list -> 'b list",
      "parameters": Array [
        Object {
          "label": Array [
            6,
            18,
          ],
        },
        Object {
          "label": Array [
            22,
            29,
          ],
        },
      ],
    },
  ],
}
`);
  });

  it("can provide signature help for an operator", async () => {
    openDocument(outdent`
      let (+) = (+)

      let _ = 1 + 2
    `);

    let items = await querySignatureHelp(Types.Position.create(2, 13));
    expect(items).toMatchInlineSnapshot(`
Object {
  "activeParameter": 1,
  "activeSignature": 0,
  "signatures": Array [
    Object {
      "label": "(+) : int -> int -> int",
      "parameters": Array [
        Object {
          "label": Array [
            6,
            9,
          ],
        },
        Object {
          "label": Array [
            13,
            16,
          ],
        },
      ],
    },
  ],
}
`);
  });

  it("can provide signature help for an anonymous function", async () => {
    openDocument(outdent`
      let _ = (fun x -> x + 1)
    `);

    let items = await querySignatureHelp(Types.Position.create(0, 26));
    expect(items).toMatchInlineSnapshot(`
Object {
  "activeParameter": 0,
  "activeSignature": 0,
  "signatures": Array [
    Object {
      "label": "_ : int -> int",
      "parameters": Array [
        Object {
          "label": Array [
            4,
            7,
          ],
        },
      ],
    },
  ],
}
`);
  });

  it("can make the non-labelled parameter active", async () => {
    openDocument(outdent`
      let map = ListLabels.map

      let _ = map []
    `);

    let items = await querySignatureHelp(Types.Position.create(2, 14));
    expect(items).toMatchInlineSnapshot(`
Object {
  "activeParameter": 1,
  "activeSignature": 0,
  "signatures": Array [
    Object {
      "label": "map : f:('a -> 'b) -> 'a list -> 'b list",
      "parameters": Array [
        Object {
          "label": Array [
            6,
            18,
          ],
        },
        Object {
          "label": Array [
            22,
            29,
          ],
        },
      ],
    },
  ],
}
`);
  });

  it("can make the labelled parameter active", async () => {
    openDocument(outdent`
      let map = ListLabels.map

      let _ = map ~f:Int.abs
    `);

    let items = await querySignatureHelp(Types.Position.create(2, 22));
    expect(items).toMatchInlineSnapshot(`
Object {
  "activeParameter": 0,
  "activeSignature": 0,
  "signatures": Array [
    Object {
      "label": "map : f:(int -> int) -> int list -> int list",
      "parameters": Array [
        Object {
          "label": Array [
            6,
            20,
          ],
        },
        Object {
          "label": Array [
            24,
            32,
          ],
        },
      ],
    },
  ],
}
`);
  });

  it("can make a labelled parameter active by prefix", async () => {
    openDocument(outdent`
      let mem = ListLabels.mem

      let _ = mem ~se
    `);

    let items = await querySignatureHelp(Types.Position.create(2, 15));
    expect(items).toMatchInlineSnapshot(`
Object {
  "activeParameter": 1,
  "activeSignature": 0,
  "signatures": Array [
    Object {
      "label": "mem : 'a -> set:'a list -> bool",
      "parameters": Array [
        Object {
          "label": Array [
            6,
            8,
          ],
        },
        Object {
          "label": Array [
            12,
            23,
          ],
        },
      ],
    },
  ],
}
`);
  });

  it("can make an optional parameter active by prefix", async () => {
    openDocument(outdent`
      let create = Hashtbl.create

      let _ = create ?ra
    `);

    let items = await querySignatureHelp(Types.Position.create(2, 18));
    expect(items).toMatchInlineSnapshot(`
Object {
  "activeParameter": 0,
  "activeSignature": 0,
  "signatures": Array [
    Object {
      "label": "create : ?random:bool -> int -> ('a, 'b) Hashtbl.t",
      "parameters": Array [
        Object {
          "label": Array [
            9,
            21,
          ],
        },
        Object {
          "label": Array [
            25,
            28,
          ],
        },
      ],
    },
  ],
}
`);
  });

  it("can return documentation for the function being applied", async () => {
    openDocument(
      outdent`
      (** This is an example of a docstring that demonstrates various ocamldoc syntax features.

          {3 Sections and Labels}

          We can create sections using {3 Section title} and labels using {3:label_name Section title with label}

          {3 Links and Cross-references}

          External links: {{:https://ocaml.org/} OCaml's official website}

          Cross-references: {!List.length} {{!List.length} Replacement text}

          {3 Inline Formatting}

          {b Bold}, {i Italic}, {e Emphasize}, {^ Superscript}, {_ Subscript}, and [inline code]

          {3 Text Alignment}

          {C Centered text}
          {L Left-aligned text}
          {R Right-aligned text}

          {3 Lists}

          {ol
          {- Ordered list item 1}
          {- Ordered list item 2}
          }

          {ul
          {- Unordered list item 1}
          {- Unordered list item 2}
          }

          - Unordered list item 1
          - Unordered list item 2

          {3 Code Blocks}

          {[
            let square x = x * x
            let result = square 3
          ]}

          {@python[
          def f():
            return 0
          ]}

          {3 Verbatim}

          {v
          This text will be displayed verbatim.
          No formatting will be applied.
          v}

          {3 Module List}

          {!modules: Array List String}

          @param x dividend
          @param divisor

          @return {i quotient}, i.e. result of division
          @raise Division_by_zero raised when divided by zero

          @see <https://en.wikipedia.org/wiki/Arithmetic#Division_(%C3%B7,_or_/)> article
          @see 'arithmetic.ml' for more context

          @since 4.0.0
          @before 4.4.0

          @deprecated use [(/)]

          @version 1.0.0
          @author John Doe *)
      let div x y =
        x / y

      let _ = div 1
    `,
    );

    let items = await querySignatureHelp(Types.Position.create(80, 13));
    expect(items).toMatchInlineSnapshot(`
Object {
  "activeParameter": 0,
  "activeSignature": 0,
  "signatures": Array [
    Object {
      "documentation": Object {
        "kind": "markdown",
        "value": "This is an example of a docstring that demonstrates various ocamldoc syntax features.

#### Sections and Labels

We can create sections using 

#### Section title

and labels using 

#### Section title with label

#### Links and Cross-references

External links: [OCaml's official website](https://ocaml.org/)

Cross-references: \`List.length\` Replacement text

#### Inline Formatting

**Bold**, *Italic*, *Emphasize*, ^{Superscript}, \\\\_{Subscript}, and \`inline code\`

#### Text Alignment

Centered text

Left-aligned text

Right-aligned text

#### Lists

1. Ordered list item 1
2. Ordered list item 2

- Unordered list item 1
- Unordered list item 2

- Unordered list item 1
- Unordered list item 2

#### Code Blocks

\`\`\`ocaml
let square x = x * x
let result = square 3
\`\`\`

\`\`\`python
def f():
  return 0
\`\`\`

#### Verbatim

\`\`\`verb
    This text will be displayed verbatim.
    No formatting will be applied.
\`\`\`

#### Module List

* Array
* List
* String

***@param*** \`x\`
dividend

***@param*** divisor

***@return***
*quotient*, i.e. result of division

***@raise*** \`Division_by_zero\`
raised when divided by zero

***@see*** [link](https://en.wikipedia.org/wiki/Arithmetic#Division_\\\\(%C3%B7,_or_/\\\\))
article

***@see*** \`arithmetic.ml\`
for more context

***@since*** \`4.0.0\`

***@before*** \`4.4.0\`

***@deprecated***
use \`(/)\`

***@version*** \`1.0.0\`

***@author*** John Doe",
      },
      "label": "div : int -> int -> int",
      "parameters": Array [
        Object {
          "label": Array [
            6,
            9,
          ],
        },
        Object {
          "label": Array [
            13,
            16,
          ],
        },
      ],
    },
  ],
}
`);
  });
});
