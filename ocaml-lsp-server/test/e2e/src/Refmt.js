#!/usr/bin/env node

const chunks = [];
process.stdin.resume();
process.stdin.on("data", (chunk) => {
  chunks.push(chunk);
});
process.stdin.on("end", () => {
  const body = Buffer.concat(chunks);
  if (body === "special string") {
    process.stderr.write("special string passed\n");
    process.exit(1);
  } else {
    process.stdout.write(body);
  }
});
