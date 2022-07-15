(lang dune 3.0)

(name lev)

(generate_opam_files true)

(license ISC)
(maintainers "Rudi Grinberg <me@rgrinberg.com>")
(authors
  "Rudi Grinberg <me@rgrinberg.com>"
  "Ulugbek Abdullaev <ulugbekna@gmail.com>")
(source (github rgrinberg/lev))

(implicit_transitive_deps false)

(package
 (synopsis "Bindings to libev")
 (description "Low level bindings to libev")
 (name lev)
 (depends
  (ppx_expect :with-test)
  base-unix))

(package
 (synopsis "Fiber + Lev")
 (description "High level API based on dune's fibers")
 (name lev-fiber)
 (depends
  (ppx_expect :with-test)
  (ocaml (>= 4.14.0))
  lev
  dyn
  fiber
  stdune
  base-threads
  base-unix))

(package
 (synopsis "Fiber + Lev + Csexp")
 (description "Client/Servers that use csexp for transport")
 (name lev-fiber-csexp)
 (depends
  (ppx_expect :with-test)
  lev-fiber
  stdune
  dyn
  csexp
  (fiber (>= 3.1.1))
  base-unix))
