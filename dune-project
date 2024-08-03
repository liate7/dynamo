(lang dune 3.14)

(name dynamo)

(using menhir 3.0)

(generate_opam_files true)

(source
 (github liate7/dynamo))

(authors "Andrew Patterson")

(maintainers "andrewpatt7@gmail.com")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name dynamo)
 (synopsis "A dynamically-typed impure functional actors language")
 (description
   "A dynamically-typed impure functional actors language, \
   inspired by ocaml, scheme, erlang, and e")
 (depends
   zarith menhir sedlex eio eio_linux eio_main containers cmdliner ppx_expect ppx_inline_test
   ppx_string ppx_show fix (qcheck :with-test))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
