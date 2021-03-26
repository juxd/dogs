(lang dune 2.7)
(name dogs)
(generate_opam_files true)

(source (github juxd/dogs))
(license MIT)
(authors "Julius Nugroho")
(maintainers "jluuiss@gmail.com")

(package
  (name dogs)
  (synopsis "More functional data abstractions.")
  (version 0.0.1)
  (depends
    (travesty (>= 0.7.0))
    (base (>= v0.14.1))
    (ppx_sexp_conv (>= v0.14.2))
    (ppx_let (>= 0.14.0))))
