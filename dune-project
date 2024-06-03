(lang dune 3.15)

(name tiny_lfu)

(generate_opam_files true)

(source
 (github nickrobison/tiny-lfu))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name tiny_lfu)
 (synopsis "OCaml implementation of Tiny-LFU cache")
 (description "A longer description")
 (depends ocaml dune (alcotest :with-test))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
