(lang dune 1.11)
(name lsp)

(implicit_transitive_deps false)

(license ISC)
(maintainers "Rudi Grinberg <me@rgrinerg.com>")
(authors
 "Andrey Popp <8mayday@gmail.com>"
 "Rusty Key <iam@stfoo.ru>"
 "Louis Roché <louis@louisroche.net>"
 "Oleksiy Golovko <alexei.golovko@gmail.com>"
 "Rudi Grinberg <me@rgrinberg.com>")

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
  result
  yojson
  ppx_yojson_conv_lib
  (ocaml (>= 4.06))
  (dune (>= 1.11))))

(package
 (name ocaml-lsp-server)
 (synopsis "LSP Server for OCaml")
 (description " An LSP server for OCaml.")
 (depends
  result
  lsp
  yojson
  ppx_yojson_conv_lib
  (ocaml (>= 4.06))
  (dune (>= 1.11))))
