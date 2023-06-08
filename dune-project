(lang dune 3.7)

(name parscomb)

(generate_opam_files true)

(source
 (github Green-Wood/parscomb))

(authors "Wenqi Zhao")

(maintainers "Wenqi ZHAO")

(license MIT)

(documentation https://github.com/Green-Wood/parscomb)

(package
 (name parscomb)
 (synopsis "A toy monadic parser combinator for OCaml")
 (description
  "Parscomb is a lightweight, monadic parser combinator library for OCaml, created as a toy project to learn and explore monadic programming. It allows you to easily create, modify, and combine parsers for various text formats and languages. The library provides essential atomic operations, operators, and derived operations, as well as several useful parsers for common tasks.")
 (depends
  (ocaml
   (>= 4.13.1))
  (dune
   (>= 3.7))
  (core
   (>= v0.15.1))
  (core_unix
   (>= v0.15.2))
  (core_bench
   (>= v0.15.0))
  (yojson
   (>= 2.0.2)))
 (tags
  (parser-combinator ocaml monadic)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
