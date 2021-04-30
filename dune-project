(lang dune 2.7)
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
  (jsonrpc (= :version))
  yojson
  ppx_yojson_conv_lib
  csexp ;; needed by stdune
  uutf
  (odoc :with-doc)
  (menhir :with-test)
  (cinaps :with-test)
  (ppx_expect (and :with-test (>= v0.14.0)))
  (ocaml (>= 4.08))))

(package
 (name ocaml-lsp-server)
 (synopsis "LSP Server for OCaml")
 (description "An LSP server for OCaml.")
 (depends
  yojson
  ppx_yojson_conv_lib
  dune-build-info
  dot-merlin-reader
  (csexp (>= 1.2.3))
  (result (>= 1.5))
  (ocamlformat :with-test)
  (ocamlfind (>= 1.5.2))
  (odoc :with-doc)
  (ocaml (and (>= 4.12) (< 4.13)))))

(package
 (name jsonrpc)
 (synopsis "Jsonrpc protocol implemenation")
 (description "See https://www.jsonrpc.org/specification")
 (depends
  (ocaml (>= 4.08))
  (odoc :with-doc)))
