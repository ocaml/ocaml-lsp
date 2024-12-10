import * as LanguageServer from "./../src/LanguageServer";

test("debug/echo", async () => {
  const languageServer = await LanguageServer.startAndInitialize();

  const params = {
    message: "testing",
  };

  const result: any = await languageServer.sendRequest("debug/echo", params);

  expect(result.message).toBe("testing");
  await LanguageServer.exit(languageServer);
});
