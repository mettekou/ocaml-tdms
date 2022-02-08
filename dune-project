(lang dune 2.0)

(name tdms)





(license ISC)

(authors
  "Dylan Meysmans")

(maintainers
  "Dylan Meysmans")

(generate_opam_files true)

(package
 (name tdms)
 (synopsis "TDMS 2.0 file support for OCaml")
 (description "TDMS 2.0 file support for OCaml")
 (depends
  (ocaml (>= 4.08.0))
  dune
  (alcotest :with-test)
  (odoc :with-doc)))
