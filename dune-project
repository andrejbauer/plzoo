(lang dune 1.11)

(using menhir 2.0)

(name plzoo)

(generate_opam_files true)

(maintainers "Andrej Bauer")

(homepage "https://plzoo.andrej.com/")

(license MIT)

(source
 (github andrejbauer/plzoo))

(package
 (name plzoo)
 (synopsis "Programming Languages Zoo")
 (description
  "The Programming Languages Zoo, or PL Zoo for short, is a collection of implementations of miniature programming languages which demonstrates various techniques used in implementation of programming languages. It is a good starting point for those who would like to implement their own programming language, or just learn how it is done.\n\nSee the [PL Zoo website](http://plzoo.andrej.com/) for further information, including installation instructions.")
 (depends
  (ocaml
   (>= 4.10.2))
  (dune
   (>= 1.11))
  base-unix
  (menhir
   (>= 20190924))))
