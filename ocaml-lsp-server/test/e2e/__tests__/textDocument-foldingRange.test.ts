import outdent from "outdent";
import * as LanguageServer from "../src/LanguageServer";

import * as Types from "vscode-languageserver-types";

describe("textDocument/foldingRange", () => {
  let languageServer = null;

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
    languageServer = null;
  });

  async function openDocument(source: string) {
    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test.ml",
        "ocaml",
        0,
        source,
      ),
    });
  }

  async function foldingRange() {
    return languageServer.sendRequest("textDocument/foldingRange", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
    });
  }

  it("returns folding ranges for `let`", async () => {
    await openDocument(outdent`
    let a = 
      let b = 1 
      in 
      let c = 
        "foo" 
      in 
      ()
    `);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 4,
          "endLine": 6,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 0,
        },
        Object {
          "endCharacter": 9,
          "endLine": 4,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 3,
        },
      ]
    `);
  });

  it("returns folding ranges for open expressions", async () => {
    await openDocument(outdent`
    open struct 
      let u = 
        ()
    end
    `);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 3,
          "endLine": 3,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 0,
        },
        Object {
          "endCharacter": 6,
          "endLine": 2,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 1,
        },
      ]
    `);
  });

  it("returns folding ranges for Pexp_apply expressions", async () => {
    await openDocument(outdent`
    Stdlib.print_endline "one
    two
    three"`);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 6,
          "endLine": 2,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 0,
        },
      ]
    `);
  });

  it("returns folding ranges for match expressions", async () => {
    await openDocument(outdent`
    match 
      Some 
        "large expr" 
    with 
    | None -> 
      () 
    | Some _ -> 
      print_endline "foo";
      print_endline "bar"
    `);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 21,
          "endLine": 8,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 0,
        },
        Object {
          "endCharacter": 4,
          "endLine": 5,
          "kind": "region",
          "startCharacter": 6,
          "startLine": 4,
        },
        Object {
          "endCharacter": 21,
          "endLine": 8,
          "kind": "region",
          "startCharacter": 8,
          "startLine": 6,
        },
      ]
    `);
  });

  it("returns folding ranges for records", async () => {
    await openDocument(outdent`
    type r = { 
      a: string;
      b: int
    }

    let f 
          { a; 
            b } = 
      { a = a;
        b = b + 1 }
    `);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 1,
          "endLine": 3,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 0,
        },
        Object {
          "endCharacter": 15,
          "endLine": 9,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 5,
        },
        Object {
          "endCharacter": 11,
          "endLine": 7,
          "kind": "region",
          "startCharacter": 6,
          "startLine": 6,
        },
        Object {
          "endCharacter": 15,
          "endLine": 9,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 8,
        },
      ]
    `);
  });

  it("returns folding ranges for classes", async () => {
    await openDocument(outdent`
    class foobar =
      let a =
        let () = Stdlib.print_endline "" in
        let () = Stdlib.print_endline "" in
        let () = Stdlib.print_endline "" in
        ()
      in
      object
        method add x y =
          let z =
            let a = 5 in
            let b = 6 in
            let () =
              let () = Stdlib.print_endline "" in
              let () = Stdlib.print_endline "" in
              let () = Stdlib.print_endline "" in
              ()
            in
            a + b
          in
          x + y + z
      end
    `);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 5,
          "endLine": 21,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 0,
        },
        Object {
          "endCharacter": 6,
          "endLine": 5,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 1,
        },
        Object {
          "endCharacter": 15,
          "endLine": 20,
          "kind": "region",
          "startCharacter": 4,
          "startLine": 8,
        },
        Object {
          "endCharacter": 13,
          "endLine": 18,
          "kind": "region",
          "startCharacter": 6,
          "startLine": 9,
        },
        Object {
          "endCharacter": 12,
          "endLine": 16,
          "kind": "region",
          "startCharacter": 8,
          "startLine": 12,
        },
      ]
    `);
  });

  it("traverses Pexp_sequence nodes", async () => {
    await openDocument(outdent`
    let a = 
      Stdlib.print_endline "";
      let b = 
        5 + 3 in
      b
    `);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 3,
          "endLine": 4,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 0,
        },
        Object {
          "endCharacter": 9,
          "endLine": 3,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 2,
        },
      ]
    `);
  });
  it("supports if/else", async () => {
    await openDocument(outdent`
    let fn a =
      if a == true then
        let () =
          Stdlib.print_endline "";
          Stdlib.print_endline ""
        in
        let () =
          Stdlib.print_endline "";
          Stdlib.print_endline ""
        in
        ()
      else
        let () =
          Stdlib.print_endline "";
          Stdlib.print_endline ""
        in
        let () =
          Stdlib.print_endline "";
          Stdlib.print_endline ""
        in
        ()
    `);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 6,
          "endLine": 20,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 0,
        },
        Object {
          "endCharacter": 29,
          "endLine": 4,
          "kind": "region",
          "startCharacter": 4,
          "startLine": 2,
        },
        Object {
          "endCharacter": 29,
          "endLine": 8,
          "kind": "region",
          "startCharacter": 4,
          "startLine": 6,
        },
        Object {
          "endCharacter": 29,
          "endLine": 14,
          "kind": "region",
          "startCharacter": 4,
          "startLine": 12,
        },
        Object {
          "endCharacter": 29,
          "endLine": 18,
          "kind": "region",
          "startCharacter": 4,
          "startLine": 16,
        },
      ]
    `);
  });

  it("supports return type annotation", async () => {
    await openDocument(outdent`
    let fn a b : int = 
      let result = 
        Stdlib.print_endline "";
        a + b 
      in
      result
    `);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 8,
          "endLine": 5,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 0,
        },
        Object {
          "endCharacter": 9,
          "endLine": 3,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 1,
        },
      ]
    `);
  });

  it("returns folding ranges for try/with", async () => {
    await openDocument(outdent`
    let result =
      try
        let () =
          Stdlib.print_endline "";
          Stdlib.print_endline ""
        in
        Some 6
      with
      | Not_found ->
        let () =
          Stdlib.print_endline "";
          Stdlib.print_endline ""
        in
        None
    `);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 8,
          "endLine": 13,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 0,
        },
        Object {
          "endCharacter": 8,
          "endLine": 13,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 1,
        },
        Object {
          "endCharacter": 29,
          "endLine": 4,
          "kind": "region",
          "startCharacter": 4,
          "startLine": 2,
        },
        Object {
          "endCharacter": 8,
          "endLine": 13,
          "kind": "region",
          "startCharacter": 13,
          "startLine": 8,
        },
        Object {
          "endCharacter": 29,
          "endLine": 11,
          "kind": "region",
          "startCharacter": 4,
          "startLine": 9,
        },
      ]
    `);
  });

  it("traverses Pexp_construct", async () => {
    await openDocument(outdent`
    let a =
      Some
        (let () =
           Stdlib.print_endline "";
           Stdlib.print_endline "";
           Stdlib.print_endline ""
         in
         5)
    `);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 7,
          "endLine": 7,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 0,
        },
        Object {
          "endCharacter": 30,
          "endLine": 5,
          "kind": "region",
          "startCharacter": 5,
          "startLine": 2,
        },
      ]
    `);
  });

  it("returns folding ranges for modules", async () => {
    await openDocument(outdent`
          module type X = sig
            type t =
              | A
              | B
              | C
          end

          module X = struct
            module type T = sig
              module T1 : sig
                type t =
                  | A
                  | B
                  | C
              end
              type t
              val uri : t -> Uri.t
              val dispose : t -> unit
            end

            module Y = struct
              let bar a =
                match a with
                | None ->
                  Stdlib.print_endline "";
                  Stdlib.print_endline "";
                  Stdlib.print_endline ""
                | Some b ->
                  Stdlib.print_endline b;
                  Stdlib.print_endline b;
                  Stdlib.print_endline b
            end

            let foo () =
              let x =
                let y = 5 in
                let z = 3 in
                let open Stdlib in
                let () =
                  let () = print_endline "" in
                  let () = print_endline "" in
                  let () = print_endline "" in
                  ()
                in
                let open Promise.Syntax in
                let%lwt result =
                  let a = 5 in
                  Promise.resolve a
                in
                let+ result_letop =
                  let a = 5 in
                  Promise.resolve a
                in
                z + y
              in
              x + 3
          end
        `);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 3,
          "endLine": 5,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 0,
        },
        Object {
          "endCharacter": 7,
          "endLine": 4,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 1,
        },
        Object {
          "endCharacter": 3,
          "endLine": 56,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 7,
        },
        Object {
          "endCharacter": 5,
          "endLine": 18,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 8,
        },
        Object {
          "endCharacter": 7,
          "endLine": 14,
          "kind": "region",
          "startCharacter": 4,
          "startLine": 9,
        },
        Object {
          "endCharacter": 11,
          "endLine": 13,
          "kind": "region",
          "startCharacter": 6,
          "startLine": 10,
        },
        Object {
          "endCharacter": 5,
          "endLine": 31,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 20,
        },
        Object {
          "endCharacter": 30,
          "endLine": 30,
          "kind": "region",
          "startCharacter": 4,
          "startLine": 21,
        },
        Object {
          "endCharacter": 30,
          "endLine": 30,
          "kind": "region",
          "startCharacter": 6,
          "startLine": 22,
        },
        Object {
          "endCharacter": 31,
          "endLine": 26,
          "kind": "region",
          "startCharacter": 12,
          "startLine": 23,
        },
        Object {
          "endCharacter": 30,
          "endLine": 30,
          "kind": "region",
          "startCharacter": 14,
          "startLine": 27,
        },
        Object {
          "endCharacter": 9,
          "endLine": 55,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 33,
        },
        Object {
          "endCharacter": 11,
          "endLine": 53,
          "kind": "region",
          "startCharacter": 4,
          "startLine": 34,
        },
        Object {
          "endCharacter": 10,
          "endLine": 42,
          "kind": "region",
          "startCharacter": 6,
          "startLine": 38,
        },
        Object {
          "endCharacter": 25,
          "endLine": 47,
          "kind": "region",
          "startCharacter": 6,
          "startLine": 45,
        },
        Object {
          "endCharacter": 11,
          "endLine": 53,
          "kind": "region",
          "startCharacter": 6,
          "startLine": 49,
        },
      ]
    `);
  });
});
