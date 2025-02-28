(lang dune 3.16)
(using menhir 3.0)

(name c11parser_ng)

(generate_opam_files true)

(source
 (github smuenzel/c11parser-ng))

(authors "Stefan Muenzel" "Jacques-Henri Jourdan" "Francois Pottier")

(maintainers "Stefan Muenzel")

(license BSD-3-Clause)

(documentation https://url/to/documentation)

(package
 (name c11parser_ng)
 (synopsis "A c11 parser")
 (description "A c11 parser")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
