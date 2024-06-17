(lang dune 3.15)

(name tiny_lfu)

(generate_opam_files true)

(source
 (github nickrobison/tiny-lfu))

(authors "Nick Robison")

(maintainers "Nick Robison")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name tiny_lfu)
 (synopsis "OCaml implementation of Tiny-LFU cache")
 (description "Thread-safe Ocaml implementation of TinyLFU cache. Mostly an experiment to play with KCAS and STM.")
 (depends ocaml dune zarith kcas_data fmt (qcheck :with-test) (qcheck-stm :with-test) (qcheck-alcotest :with-test) (dscheck :with-test) (alcotest :with-test) (ppx_deriving :with-test))
 (tags
  (cache kcas lfu stm)))
