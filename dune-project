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
 "cannorin <cannorin@gmail.com>"
 "Ulugbek Abdullaev <ulugbekna@gmail.com>"
 "Thibaut Mattio <thibaut.mattio@gmail.com>"
 "Max Lantas <mnxndev@outlook.com>")

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
  (ppx_yojson_conv_lib (>= "v0.14"))
  pp ;; needed by stdune
  (csexp (>= 1.4)) ;; needed by stdune
  uutf
  (odoc :with-doc)
  (ocaml (>= 4.12))))

(package
 (name ocaml-lsp-server)
 (synopsis "LSP Server for OCaml")
 (description "An LSP server for OCaml.")
 (depends
  yojson
  re
  (ppx_yojson_conv_lib (>= "v0.14"))
  dune-build-info
  dot-merlin-reader
  (pp (>= 1.1.2))
  (csexp (>= 1.4))
  (result (>= 1.5))
  (ocamlformat-rpc-lib (= 0.18.0))
  (odoc :with-doc)
  (ocaml (and (>= 4.12) (< 4.13)))))

(package
 (name jsonrpc)
 (synopsis "Jsonrpc protocol implemenation")
 (description "See https://www.jsonrpc.org/specification")
 (depends
  (ocaml (>= 4.08))
  (odoc :with-doc)))
