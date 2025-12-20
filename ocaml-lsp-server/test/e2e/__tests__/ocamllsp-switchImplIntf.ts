import { assert } from "node:console";
import { promises as fs } from "node:fs";
import * as path from "node:path";
import * as Protocol from "vscode-languageserver-protocol";
import {
  type DocumentUri,
  TextDocumentItem,
} from "vscode-languageserver-types";
import { URI } from "vscode-uri";
import * as LanguageServer from "./../src/LanguageServer";

describe("ocamllsp/switchImplIntf", () => {
  let languageServer: LanguageServer.LanguageServer;

  function openDocument(documentUri: DocumentUri) {
    languageServer.sendNotification(
      Protocol.DidOpenTextDocumentNotification.type,
      {
        textDocument: TextDocumentItem.create(documentUri, "ocaml", 0, ""),
      },
    );
  }

  /* sends request "ocamllsp/switchImplIntf" */
  async function ocamllspSwitchImplIntf(
    documentUri: DocumentUri,
  ): Promise<Array<DocumentUri>> {
    return languageServer.sendRequest("ocamllsp/switchImplIntf", documentUri);
  }

  const testWorkspacePath = path.join(__dirname, "..", "test_files/");

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
    await fs.mkdir(testWorkspacePath);
  });

  afterEach(async () => {
    await fs.rm(testWorkspacePath, { recursive: true });
    await LanguageServer.exit(languageServer);
  });

  const createPathForFile = (filename: string) =>
    path.join(testWorkspacePath, filename);

  const createFileAtPath = (path: string) =>
    fs.writeFile(path, "", { flag: "a+" });

  const pathToDocumentUri = (path: string): DocumentUri =>
    URI.file(path).toString();

  const [mli, ml, mll, mly, rei, re] = ["mli", "ml", "mll", "mly", "rei", "re"];

  const testRequest = async (
    requestParam: DocumentUri,
    expectedResponse: DocumentUri[],
  ) => {
    const response = await ocamllspSwitchImplIntf(requestParam);
    expect(response).toEqual(expectedResponse);
  };

  /**
   * For testing 'ocamllsp/switchImplIntf'
   *
   * @param extsForCreation file name extension for files to be created in
   *    (test) workspace folder. The first file created (even if only one file
   *    is created) is treated as the file a user wants to switch from.
   * @param extExpected file name extensions that are expected to be returned as
   *    a reponse to 'ocamllsp/switchImplIntf'
   */
  const testingPipeline = async (
    extsForCreation: string[],
    extExpected: string[],
  ) => {
    assert(
      extsForCreation.length > 0,
      "extensions for creation should not be empty",
    );
    assert(
      extExpected.length > 0,
      "expected response extensions should not be empty",
    );

    const filePathsForCreation = extsForCreation.map((ext) => {
      const filename = "test.".concat(ext);
      return createPathForFile(filename);
    });

    await Promise.all(filePathsForCreation.map(createFileAtPath));

    const filePathToSwitchFrom = filePathsForCreation[0];
    const fileURIToSwitchFrom = pathToDocumentUri(filePathToSwitchFrom);
    await openDocument(fileURIToSwitchFrom);

    const expectedFileURIs = extExpected.map((ext) => {
      const filename = "test.".concat(ext);
      const filePath = createPathForFile(filename);
      return pathToDocumentUri(filePath);
    });

    await testRequest(fileURIToSwitchFrom, expectedFileURIs);
  };

  /* `create`, `expect`, and `test_case` are for declarativeness */
  const varargFn = <T>(...args: T[]): T[] => args;
  const createFiles = varargFn;
  const expectSwitchTo = varargFn;
  const testCase = (filesToCreate: string[], filesToExpect: string[]) => [
    filesToCreate,
    filesToExpect,
  ];

  test.each([
    testCase(createFiles(mli), expectSwitchTo(ml)),
    testCase(createFiles(mli, ml), expectSwitchTo(ml)),
    testCase(createFiles(ml), expectSwitchTo(mli)),
    testCase(createFiles(ml, mli), expectSwitchTo(mli)),
    testCase(createFiles(mli, mll), expectSwitchTo(mll)),
    testCase(createFiles(mli, ml, mll), expectSwitchTo(ml, mll)),
  ])("test switches (%s => %s)", testingPipeline);

  it("can switch from file URI with non-file scheme", async () => {
    const mlFpath = createPathForFile("test.ml");
    await createFileAtPath(mlFpath);
    const mlUri = pathToDocumentUri(mlFpath);

    const newMliFpath = createPathForFile("test.mli");
    await createFileAtPath(newMliFpath);
    const mliUriUntitledScheme: DocumentUri = URI.file(newMliFpath)
      .with({
        scheme: "untitled",
      })
      .toString();

    testRequest(mliUriUntitledScheme, [mlUri]);
  });
});
