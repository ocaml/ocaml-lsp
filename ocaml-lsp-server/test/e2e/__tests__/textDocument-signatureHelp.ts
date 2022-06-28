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
    expect(items).toMatchObject({
      signatures: [
        {
          label: "map : f:('a -> 'b) -> 'a list -> 'b list",
          parameters: [
            {
              label: [6, 18],
            },
            {
              label: [22, 29],
            },
          ],
        },
      ],
      activeSignature: 0,
      activeParameter: 1,
    });
  });

  it("can provide signature help for an operator", async () => {
    openDocument(outdent`
      let (+) = (+)

      let _ = 1 + 2
    `);

    let items = await querySignatureHelp(Types.Position.create(2, 13));
    expect(items).toMatchObject({
      signatures: [
        {
          label: "(+) : int -> int -> int",
          parameters: [
            {
              label: [6, 9],
            },
            {
              label: [13, 16],
            },
          ],
        },
      ],
      activeSignature: 0,
      activeParameter: 1,
    });
  });

  it("can provide signature help for an anonymous function", async () => {
    openDocument(outdent`
      let _ = (fun x -> x + 1)
    `);

    let items = await querySignatureHelp(Types.Position.create(0, 26));
    expect(items).toMatchObject({
      signatures: [
        {
          label: "_ : int -> int",
          parameters: [
            {
              label: [4, 7],
            },
          ],
        },
      ],
      activeSignature: 0,
      activeParameter: 0,
    });
  });

  it("can make the non-labelled parameter active", async () => {
    openDocument(outdent`
      let map = ListLabels.map

      let _ = map []
    `);

    let items = await querySignatureHelp(Types.Position.create(2, 14));
    expect(items).toMatchObject({
      signatures: [
        {
          label: "map : f:('a -> 'b) -> 'a list -> 'b list",
          parameters: [
            {
              label: [6, 18],
            },
            {
              label: [22, 29],
            },
          ],
        },
      ],
      activeSignature: 0,
      activeParameter: 1,
    });
  });

  it("can make the labelled parameter active", async () => {
    openDocument(outdent`
      let map = ListLabels.map

      let _ = map ~f:Int.abs
    `);

    let items = await querySignatureHelp(Types.Position.create(2, 22));
    expect(items).toMatchObject({
      signatures: [
        {
          label: "map : f:(int -> int) -> int list -> int list",
          parameters: [
            {
              label: [6, 20],
            },
            {
              label: [24, 32],
            },
          ],
        },
      ],
      activeSignature: 0,
      activeParameter: 0,
    });
  });

  it("can make a labelled parameter active by prefix", async () => {
    openDocument(outdent`
      let mem = ListLabels.mem

      let _ = mem ~se
    `);

    let items = await querySignatureHelp(Types.Position.create(2, 15));
    expect(items).toMatchObject({
      signatures: [
        {
          label: "mem : 'a -> set:'a list -> bool",
          parameters: [
            {
              label: [6, 8],
            },
            {
              label: [12, 23],
            },
          ],
        },
      ],
      activeSignature: 0,
      activeParameter: 1,
    });
  });

  it("can make an optional parameter active by prefix", async () => {
    openDocument(outdent`
      let create = Hashtbl.create

      let _ = create ?ra
    `);

    let items = await querySignatureHelp(Types.Position.create(2, 18));
    expect(items).toMatchObject({
      signatures: [
        {
          label: "create : ?random:bool -> int -> ('a, 'b) Hashtbl.t",
          parameters: [
            {
              label: [9, 21],
            },
            {
              label: [25, 28],
            },
          ],
        },
      ],
      activeSignature: 0,
      activeParameter: 0,
    });
  });

  it("can return documentation for the function being applied", async () => {
    openDocument(
      outdent`
      (** This function has a nice documentation.

          It performs division of two integer numbers.

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

    let items = await querySignatureHelp(Types.Position.create(23, 13));
    expect(items).toMatchObject({
      activeSignature: 0,
      activeParameter: 0,
      signatures: [
        {
          label: "div : int -> int -> int",
          parameters: [
            {
              label: [6, 9],
            },
            {
              label: [13, 16],
            },
          ],
          documentation: {
            kind: "markdown",
            value: outdent`
              This function has a nice documentation.

              It performs division of two integer numbers.
              * * *
              ***@param*** \`x\` dividend

              ***@param*** divisor

              ***@return*** *quotient*, i.e. result of division

              ***@raise*** \`Division_by_zero\` raised when divided by zero

              ***@see*** [link](https://en.wikipedia.org/wiki/Arithmetic#Division_(%C3%B7,_or_/)) article

              ***@see*** \`arithmetic.ml\` for more context

              ***@since*** \`4.0.0\`

              ***@before*** \`4.4.0\`

              ***@deprecated*** use \`(/)\`

              ***@version*** \`1.0.0\`

              ***@author*** John Doe
              `,
          },
        },
      ],
    });
  });
});
