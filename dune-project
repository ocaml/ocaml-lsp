(lang dune 2.5)
(using menhir 2.0)
(using cinaps 1.0)
(name lsp)

(implicit_transitive_deps false)

(license ISC)
(maintainers "Rudi Grinberg <me@rgrinerg.com>")
(authors
 "Andrey Popp <8mayday@gmail.com>"
 "Rusty Key <iam@stfoo.ru>"
 "Louis Roché <louis@louisroche.net>"
 "Oleksiy Golovko <alexei.golovko@gmail.com>"
 "Rudi Grinberg <me@rgrinberg.com>"
 "Sacha Ayoun <sachaayoun@gmail.com>"
 "cannorin <cannorin@gmail.com>")

(source (github ocaml/ocaml-lsp))

(generate_opam_files true)

(package
 (name lsp)
 (synopsis "LSP protocol implementation in OCaml")
 (description "
Implementation of the LSP protocol in OCaml. It is designed to be as portable as
possible and does not make any assumptions about IO.
")
 (depends
  (base :with-test)
  (cinaps :with-test)
  (jsonrpc (= :version))
  (menhir :with-test)
  (ocaml (>= 4.06))
  (odoc :with-doc)
  (ppx_expect (and :with-test (>= v0.14.0)))
  (ppx_inline_test :with-test)
  (result (>= 1.5))
  cppo
  csexp
  ocaml-syntax-shims
  ppx_yojson_conv_lib
  stdlib-shims
  uutf
  yojson))

(package
 (name ocaml-lsp-server)
 (synopsis "LSP Server for OCaml")
 (description "An LSP server for OCaml.")
 (depends
  (base :with-test)
  (csexp (>= 1.2.3))
  (ocaml (and (>= 4.06) (< 4.12)))
  (ocamlfind (>= 1.5.2))
  (ocamlformat :with-test)
  (odoc :with-doc)
  (ppx_expect :with-test)
  (ppx_inline_test :with-test)
  (result (>= 1.5))
  dot-merlin-reader
  dune-build-info
  ppx_yojson_conv_lib
  stdlib-shims
  yojson))

(package
 (name jsonrpc)
 (synopsis "Jsonrpc protocol implemenation")
 (description "See https://www.jsonrpc.org/specification")
 (depends
  (base :with-test)
  (ocaml (>= 4.06))
  (odoc :with-doc)
  (ppx_expect :with-test)
  (ppx_inline_test :with-test)
  (result (>= 1.5))
  ocaml-syntax-shims
  ppx_yojson_conv_lib
  stdlib-shims
  yojson))
