(lang dune 3.15)

(name tiny_lfu)

(generate_opam_files true)

(source
 (github nickrobison/tiny-lfu))

(authors "Author Name")

(maintainers "Maintainer Name")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name tiny_lfu)
 (synopsis "OCaml implementation of Tiny-LFU cache")
 (description "A longer description")
 (depends ocaml dune zarith kcas_data (qcheck :with-test) (qcheck-stm :with-test) (qcheck-alcotest :with-test) (dscheck :with-test) (alcotest :with-test))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
