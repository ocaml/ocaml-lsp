import outdent from "outdent";
import * as LanguageServer from "../src/LanguageServer";
import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";

describe("textDocument/foldingRange", () => {
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

  async function foldingRange() {
    return languageServer.sendRequest("textDocument/foldingRange", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
    });
  }

  it("returns folding ranges for `let`", async () => {
    openDocument(outdent`
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
    openDocument(outdent`
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
          "endCharacter": 3,
          "endLine": 3,
          "kind": "region",
          "startCharacter": 5,
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
    openDocument(outdent`
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

  it("returns folding ranges for Pexp_letop", async () => {
    openDocument(outdent`
    let () = 
      let+ outline =
        Stdlib.print_endline "one";
        Stdlib.print_endline "two";
    in
    let symbol_info_of_outline_item =
      Stdlib.print_endline "one";
      Stdlib.print_endline "two";`);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 29,
          "endLine": 7,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 0,
        },
        Object {
          "endCharacter": 29,
          "endLine": 7,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 1,
        },
        Object {
          "endCharacter": 29,
          "endLine": 7,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 5,
        },
      ]
    `);
  });

  it("returns folding ranges for Pexp_newtype", async () => {
    openDocument(outdent`
    let magic_of_kind : type a . a ast_kind -> string = 
      let () = 
        Stdlib.print_endline "one";
        Stdlib.print_endline "two" 
      in ()`);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 7,
          "endLine": 4,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 0,
        },
        Object {
          "endCharacter": 30,
          "endLine": 3,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 1,
        },
      ]
    `);
  });

  it("returns folding ranges for type_extension", async () => {
    openDocument(outdent`
    type t +=
      | A
      | B

    module type Type = sig
      type t += 
        | A 
        | B
    end
    `);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 5,
          "endLine": 2,
          "kind": "region",
          "startCharacter": 5,
          "startLine": 0,
        },
        Object {
          "endCharacter": 3,
          "endLine": 8,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 4,
        },
        Object {
          "endCharacter": 3,
          "endLine": 8,
          "kind": "region",
          "startCharacter": 19,
          "startLine": 4,
        },
        Object {
          "endCharacter": 7,
          "endLine": 7,
          "kind": "region",
          "startCharacter": 7,
          "startLine": 5,
        },
      ]
    `);
  });

  it("returns folding ranges for match expressions", async () => {
    openDocument(outdent`
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
    openDocument(outdent`
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
    openDocument(outdent`
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
          "endCharacter": 5,
          "endLine": 21,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 1,
        },
        Object {
          "endCharacter": 6,
          "endLine": 5,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 1,
        },
        Object {
          "endCharacter": 5,
          "endLine": 21,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 7,
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
  it("returns folding ranges Pexp_while", async () => {
    openDocument(outdent`
    while true do
      Stdlib.print_endline "one";
      Stdlib.print_endline "two";
    done
    `);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 4,
          "endLine": 3,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 0,
        },
      ]
    `);
  });

  it("returns folding ranges Pexp_for", async () => {
    openDocument(outdent`
    for j = 0 to Array.length index - 1 do
      Stdlib.print_endline "one";
      Stdlib.print_endline "two";
    done;
    `);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 4,
          "endLine": 3,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 0,
        },
      ]
    `);
  });

  it("returns folding ranges Pexp_object", async () => {
    openDocument(outdent`
    object
      method print =
        Stdlib.print_enline "one";
        Stdlib.print_enline "two";
    end
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
          "endCharacter": 30,
          "endLine": 3,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 1,
        },
      ]
    `);
  });

  it("returns folding ranges Pexp_pack", async () => {
    openDocument(outdent`
    (module Set.Make (struct
      type t = s
      let compare = cmp
      let print =
        Stdlib.print_endline "one";
        Stdlib.print_endline "two"
    end))
    `);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 5,
          "endLine": 6,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 0,
        },
        Object {
          "endCharacter": 3,
          "endLine": 6,
          "kind": "region",
          "startCharacter": 18,
          "startLine": 0,
        },
        Object {
          "endCharacter": 30,
          "endLine": 5,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 3,
        },
      ]
    `);
  });

  it("returns folding ranges Pexp_letmodule", async () => {
    openDocument(outdent`
    let module W = Set.Make (struct
      type t = s

      let compare = cmp

      let print =
        Stdlib.print_endline "one";
        Stdlib.print_endline "two"
    end)
    `);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 4,
          "endLine": 8,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 0,
        },
        Object {
          "endCharacter": 3,
          "endLine": 8,
          "kind": "region",
          "startCharacter": 25,
          "startLine": 0,
        },
        Object {
          "endCharacter": 30,
          "endLine": 7,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 5,
        },
      ]
    `);
  });

  it("returns folding ranges for value_description", async () => {
    openDocument(outdent`
    module Type : sig
      val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
        ?params:(core_type * (variance * injectivity)) list ->
        ?cstrs:(core_type * core_type * loc) list ->
        ?kind:type_kind -> ?priv:private_flag -> ?manifest:core_type -> str ->
        type_declaration
    end
    `);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 3,
          "endLine": 6,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 0,
        },
        Object {
          "endCharacter": 3,
          "endLine": 6,
          "kind": "region",
          "startCharacter": 14,
          "startLine": 0,
        },
        Object {
          "endCharacter": 20,
          "endLine": 5,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 1,
        },
      ]
    `);
  });

  it("returns folding ranges for Pstr_extension", async () => {
    openDocument(outdent`
    [%%expect{|
      module type Module =
        sig
          module N : sig type t end
          type t
        end
      |}]
    `);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 5,
          "endLine": 6,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 0,
        },
      ]
    `);
  });

  it("traverses Pexp_lazy nodes", async () => {
    openDocument(outdent`
    let res =
      lazy
        (let () =
           Stdlib.print_endline "one";
           Stdlib.print_endline "two"
         in
         ())
    in
    `);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 8,
          "endLine": 6,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 0,
        },
        Object {
          "endCharacter": 33,
          "endLine": 4,
          "kind": "region",
          "startCharacter": 5,
          "startLine": 2,
        },
      ]
    `);
  });

  it("traverses Pexp_letexception nodes", async () => {
    openDocument(outdent`
    let decode_map str =
      let exception Shortcut of error_message in
      let () =
        Stdlib.print_endline "one";
        Stdlib.print_endline "two"
      in
      ()
    in
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
          "endCharacter": 30,
          "endLine": 4,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 2,
        },
      ]
    `);
  });

  it("traverses Pexp_sequence nodes", async () => {
    openDocument(outdent`
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
    openDocument(outdent`
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
          "endCharacter": 6,
          "endLine": 10,
          "kind": "region",
          "startCharacter": 5,
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
    openDocument(outdent`
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
    openDocument(outdent`
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
    openDocument(outdent`
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
    openDocument(outdent`
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
          "endCharacter": 3,
          "endLine": 5,
          "kind": "region",
          "startCharacter": 16,
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
          "endCharacter": 3,
          "endLine": 56,
          "kind": "region",
          "startCharacter": 11,
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
          "endCharacter": 5,
          "endLine": 18,
          "kind": "region",
          "startCharacter": 18,
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
          "endCharacter": 7,
          "endLine": 14,
          "kind": "region",
          "startCharacter": 16,
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
          "endCharacter": 5,
          "endLine": 31,
          "kind": "region",
          "startCharacter": 13,
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

  it("traverses Pstr_extension structure item", async () => {
    openDocument(outdent`
    let%expect_test "test from jsonrpc_test.ml" =
      let a =
        let b = 5 in
        6 + 5
      in
      Stdlib.print_endline (string_of_int 5)
    `);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 40,
          "endLine": 5,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 0,
        },
        Object {
          "endCharacter": 40,
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

  it("returns folding ranges for class_type", async () => {
    openDocument(outdent`
    class type foo_t =
    object
      inherit castable
      method foo: string
    end;;
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
          "endCharacter": 3,
          "endLine": 4,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 1,
        },
      ]
    `);
  });

  it("returns folding ranges for class_type_field", async () => {
    openDocument(outdent`
    module type Type = sig
      class reload_generic :
        object
          method select_operation :
               Cmm.operation
            -> Cmm.expression list
            -> Debuginfo.t
            -> Mach.operation * Cmm.expression list
        end
    end
    `);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 3,
          "endLine": 9,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 0,
        },
        Object {
          "endCharacter": 3,
          "endLine": 9,
          "kind": "region",
          "startCharacter": 19,
          "startLine": 0,
        },
        Object {
          "endCharacter": 7,
          "endLine": 8,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 1,
        },
        Object {
          "endCharacter": 7,
          "endLine": 8,
          "kind": "region",
          "startCharacter": 4,
          "startLine": 2,
        },
        Object {
          "endCharacter": 47,
          "endLine": 7,
          "kind": "region",
          "startCharacter": 6,
          "startLine": 3,
        },
      ]
    `);
  });

  it("returns folding ranges for class_description", async () => {
    openDocument(outdent`
    module type T = sig
      class cse_generic : 
        object 
        end
    end
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
          "endCharacter": 3,
          "endLine": 4,
          "kind": "region",
          "startCharacter": 16,
          "startLine": 0,
        },
        Object {
          "endCharacter": 7,
          "endLine": 3,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 1,
        },
        Object {
          "endCharacter": 7,
          "endLine": 3,
          "kind": "region",
          "startCharacter": 4,
          "startLine": 2,
        },
      ]
    `);
  });

  it("returns folding ranges for class_expr", async () => {
    openDocument(outdent`
    class x =
      object
        val x = 3
        val virtual x : t
        val! mutable x = 3
      end
    `);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 5,
          "endLine": 5,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 0,
        },
        Object {
          "endCharacter": 5,
          "endLine": 5,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 1,
        },
      ]
    `);
  });

  it("returns folding ranges for class_type", async () => {
    openDocument(outdent`
    class type t =
      object
        val x : t
        val mutable x : t
      end
    `);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 5,
          "endLine": 4,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 0,
        },
        Object {
          "endCharacter": 5,
          "endLine": 4,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 1,
        },
      ]
    `);
  });

  it("returns folding ranges for Pmod_functor and Pmod_structure", async () => {
    openDocument(outdent`
    module M =
      functor (M : S) ->
        (val x)
        (struct 
          type t = int
          let x = 
            Stdlib.print_endline "one";
            Stdlib.print_endline "two";
        end)
    `);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 8,
          "endLine": 8,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 0,
        },
        Object {
          "endCharacter": 8,
          "endLine": 8,
          "kind": "region",
          "startCharacter": 10,
          "startLine": 1,
        },
        Object {
          "endCharacter": 7,
          "endLine": 8,
          "kind": "region",
          "startCharacter": 5,
          "startLine": 3,
        },
        Object {
          "endCharacter": 35,
          "endLine": 7,
          "kind": "region",
          "startCharacter": 6,
          "startLine": 5,
        },
      ]
    `);
  });

  it("returns folding ranges for Pmty_functor and Pmty_signature", async () => {
    openDocument(outdent`
    module type S = 
      functor (M : S) (_ : module type of M) -> 
        sig
          type t =
            | A
            | B
        end
    `);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 7,
          "endLine": 6,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 0,
        },
        Object {
          "endCharacter": 7,
          "endLine": 6,
          "kind": "region",
          "startCharacter": 10,
          "startLine": 1,
        },
        Object {
          "endCharacter": 7,
          "endLine": 6,
          "kind": "region",
          "startCharacter": 18,
          "startLine": 1,
        },
        Object {
          "endCharacter": 7,
          "endLine": 6,
          "kind": "region",
          "startCharacter": 4,
          "startLine": 2,
        },
        Object {
          "endCharacter": 11,
          "endLine": 5,
          "kind": "region",
          "startCharacter": 6,
          "startLine": 3,
        },
      ]
    `);
  });

  it("returns folding ranges for Pexp_ifthenelse", async () => {
    openDocument(outdent`
    if tool_name = "ocamldep" then
      if is_self_reference ~input_name ~loc lid then
        {type_decl with ptype_manifest = None}
      else {type_decl with ptype_manifest = Some manifest}
    else
      let x =
        let () = Stdlib.print_endline "one" in
        let () = Stdlib.print_endline "two" in
        ()
      in
      let y =
        let () = Stdlib.print_endline "one" in
        let () = Stdlib.print_endline "two" in
        ()
      in
      ()
    `);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 54,
          "endLine": 3,
          "kind": "region",
          "startCharacter": 3,
          "startLine": 0,
        },
        Object {
          "endCharacter": 42,
          "endLine": 2,
          "kind": "region",
          "startCharacter": 5,
          "startLine": 1,
        },
        Object {
          "endCharacter": 6,
          "endLine": 8,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 5,
        },
        Object {
          "endCharacter": 6,
          "endLine": 13,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 10,
        },
      ]
    `);
  });
});
