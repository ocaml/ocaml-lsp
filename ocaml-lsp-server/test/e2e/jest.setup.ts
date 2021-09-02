import { toEqualUri } from "./src/LanguageServer";

expect.extend({
  toEqualUri,
});

declare global {
  namespace jest {
    interface Matchers<R> {
      toEqualUri(uri: string): R;
    }
  }
}
