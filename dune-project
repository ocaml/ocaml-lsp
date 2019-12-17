(lang dune 1.11)
(name lsp)

(implicit_transitive_deps false)

(maintainers "me@rgrinberg.com")

(authors
 "Andrey Popp"
 "Dave Aitken"
 "Rusty Key"
 "Louis Roché"
 "Oleksiy Golovko"
 "Rudi Grinberg")

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
  yojson
  (ocaml (>= 4.06))
  (dune (>= 1.11))))

(package
 (name ocaml-lsp-server)
 (synopsis "LSP Server for OCaml")
 (description " An LSP server for OCaml.")
 (depends
  lsp
  (ocaml (>= 4.06))
  (dune (>= 1.11))
  lsp))
