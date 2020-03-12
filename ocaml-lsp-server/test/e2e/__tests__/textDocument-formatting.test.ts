import outdent from "outdent";
import * as LanguageServer from "../src/LanguageServer";

import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";
import * as fs from "fs";
import * as path from "path";
import * as os from "os";

import { execSync } from "child_process";

const ocamlFormat = `
break-cases=all
break-separators=before
break-sequences=true
cases-exp-indent=2
doc-comments=before
dock-collection-brackets=false
field-space=loose
if-then-else=k-r
indicate-nested-or-patterns=unsafe-no
let-and=sparse
sequence-style=terminator
space-around-arrays
space-around-lists
space-around-records
type-decl=sparse
wrap-comments=true
`;

function setupOcamlFormat(ocamlFormat: string) {
  let tmpdir = fs.mkdtempSync(path.join(os.tmpdir(), "ocamllsp-test-"));
  fs.writeFileSync(path.join(tmpdir, ".ocamlformat"), ocamlFormat);
  return tmpdir;
}

function setupRefmt() {
  let tmpdir = fs.mkdtempSync(path.join(os.tmpdir(), "ocamllsp-test-"));
  return tmpdir;
}

async function openDocument(languageServer, source, name) {
  await languageServer.sendNotification("textDocument/didOpen", {
    textDocument: Types.TextDocumentItem.create(name, "txt", 0, source),
  });
}

async function query(languageServer, name) {
  return await languageServer.sendRequest("textDocument/formatting", {
    textDocument: Types.TextDocumentIdentifier.create(name),
    options: Types.FormattingOptions.create(2, true),
  });
}

function link() {
  execSync("yarn link");
}

function unlink() {
  execSync("yarn unlink");
}

describe("textDocument/formatting", () => {
  describe("reformatter binary present", () => {
    let languageServer = null;

    beforeAll(() => {
      link();
    });

    afterAll(() => {
      unlink();
    });

    afterEach(async () => {
      await LanguageServer.exit(languageServer);
      languageServer = null;
    });

    it("can format an ocaml impl file", async () => {
      languageServer = await LanguageServer.startAndInitialize();

      let name = path.join(setupOcamlFormat(ocamlFormat), "test.ml");

      await openDocument(
        languageServer,
        "let rec gcd a b =\n" +
          "  match (a, b) with\n" +
          "  | 0, n\n" +
          "  | n, 0 ->\n" +
          "    n\n" +
          "  | _, _ -> gcd a (b mod a)\n",
        name,
      );

      let result = await query(languageServer, name);
      expect(result).toMatchObject([
        {
          newText:
            "let rec gcd a b =\n" +
            "  match (a, b) with\n" +
            "  | 0, n\n" +
            "  | n, 0 ->\n" +
            "    n\n" +
            "  | _, _ -> gcd a (b mod a)\n",
        },
      ]);
    });

    it("can format an ocaml intf file", async () => {
      languageServer = await LanguageServer.startAndInitialize();

      let name = path.join(setupOcamlFormat(ocamlFormat), "test.mli");

      await openDocument(
        languageServer,
        "module Test : sig\n  type t =\n    | Foo\n    | Bar\n    | Baz\nend\n",
        name,
      );

      let result = await query(languageServer, name);

      expect(result).toMatchObject([
        {
          newText:
            "module Test : sig\n  type t =\n    | Foo\n    | Bar\n    | Baz\nend\n",
        },
      ]);
    });

    it("can format a reason impl file", async () => {
      languageServer = await LanguageServer.startAndInitialize();

      let name = path.join(setupOcamlFormat(ocamlFormat), "test.re");

      await openDocument(
        languageServer,
        "let rec gcd = (a, b) =>\n" +
          "  switch (a, b) {\n" +
          "  | (0, n)\n" +
          "  | (n, 0) => n\n" +
          "  | (_, _) => gcd(a, b mod a)\n" +
          "  };\n",
        name,
      );

      let result = await query(languageServer, name);
      expect(result).toMatchObject([
        {
          newText:
            "let rec gcd = (a, b) =>\n" +
            "  switch (a, b) {\n" +
            "  | (0, n)\n" +
            "  | (n, 0) => n\n" +
            "  | (_, _) => gcd(a, b mod a)\n" +
            "  };\n",
        },
      ]);
    });

    it("can format a reason intf file", async () => {
      languageServer = await LanguageServer.startAndInitialize();

      let name = path.join(setupOcamlFormat(ocamlFormat), "test.rei");

      await openDocument(
        languageServer,
        "module Test: {\n" +
          "  type t =\n" +
          "    | Foo\n" +
          "    | Bar\n" +
          "    | Baz;\n" +
          "};\n",
        name,
      );

      let result = await query(languageServer, name);

      expect(result).toMatchObject([
        {
          newText:
            "module Test: {\n" +
            "  type t =\n" +
            "    | Foo\n" +
            "    | Bar\n" +
            "    | Baz;\n" +
            "};\n",
        },
      ]);
    });
  });

  describe("reformatter binary absent", () => {
    let languageServer = null;

    beforeAll(() => {
      link();
    });

    afterAll(() => {
      unlink();
    });

    afterEach(async () => {
      await LanguageServer.exit(languageServer);
      languageServer = null;
    });

    it("formatting an ocaml file throws an InvalidRequest error", async () => {
      languageServer = await LanguageServer.startAndInitialize();

      let name = path.join(setupOcamlFormat(ocamlFormat), "test.mli");

      await openDocument(
        languageServer,
        outdent`special string`,
        name,
      );

      try {
        await query(languageServer, name);
        fail("should throw an error the line before");
      } catch (e) {
        expect(e.code).toEqual(Protocol.ErrorCodes.InternalError);
        expect(e.message).toBe("special string passed");
      }
    });

    it("formatting a reason file throws an InvalidRequest error", async () => {
      languageServer = await LanguageServer.startAndInitialize();

      let name = path.join(setupOcamlFormat(ocamlFormat), "test.rei");

      await openDocument(
        languageServer,
        "special string",
        name,
      );

      try {
        await query(languageServer, name);
        fail("should throw an error the line before");
      } catch (e) {
        expect(e.code).toEqual(Protocol.ErrorCodes.InternalError);
        expect(e.message).toBe("special string passed");
      }
    });
  });
});
