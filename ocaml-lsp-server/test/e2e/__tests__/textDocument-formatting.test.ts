import outdent from "outdent";
import * as LanguageServer from "../src/LanguageServer";

import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";
import * as fs from "fs";
import * as path from "path";
import * as os from "os";

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

describe("textDocument/formatting", () => {
  describe("reformatter binary present", () => {
    let languageServer = null;

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
  });
});
