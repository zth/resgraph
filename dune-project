(lang dune 2.3)

(generate_opam_files true)

(authors "Gabriel Nordeborn")

(maintainers "Gabriel Nordeborn")

(homepage "https://github.com/rescript-lang/rescript-vscode")

(bug_reports "https://github.com/rescript-lang/rescript-vscode/issues")

(package
 (name resgraph)
 (synopsis "ResGraph")
 (depends
  (ocaml
   (>= 4.10))
  (ocamlformat
   (= 0.22.4))
  (cppo
   (= 1.6.9))
  (reanalyze
   (= 2.23.0))
  dune))
