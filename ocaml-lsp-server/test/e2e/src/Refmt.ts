#!/usr/bin/env node

const [, , ...args] = process.argv

if (args.includes("--interface=true")) {
    console.log(
        "module Test: {\n" +
        "  type t =\n" +
        "    | Foo\n" +
        "    | Bar\n" +
        "    | Baz;\n" +
        "};")
} else console.log(
    "let rec gcd = (a, b) =>\n" +
    "  switch (a, b) {\n" +
    "  | (0, n)\n" +
    "  | (n, 0) => n\n" +
    "  | (_, _) => gcd(a, b mod a)\n" +
    "  };")