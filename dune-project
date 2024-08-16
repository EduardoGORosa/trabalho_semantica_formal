(lang dune 3.16)

(name l1interpreter)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name l1interpreter)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
   ocaml
   dune
   utop)
 (tags
  (topics "to describe" your project))
 (allow_empty))
; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
