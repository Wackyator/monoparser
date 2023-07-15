(lang dune 3.9)

(name monoparser)

(generate_opam_files true)

(source
 (github Wackyator/monoparser))

(authors "Wackyator")

(maintainers "Wackyator")

(license LICENSE)

(documentation https://github.com/Wackyator/monoparser)

(package
 (name monoparser)
 (synopsis "Monadic Parser Combinator")
 (description "Monadic Parser Combinator")
 (depends ocaml dune)
 (tags
  ("Parser" "Parser Combinator" "Monadic Parser Combinator")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
