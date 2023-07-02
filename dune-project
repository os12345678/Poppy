(lang dune 3.5)

(name poppy)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(using menhir 2.1)

(package
 (name poppy)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  core
  ppx_jane
  ppx_expect
  ppx_inline_test
  ppx_let
  menhir
  menhirLib
  llvm
  ppx_deriving
  fmt)
 (tags
  (topics "to describe" your project)))
