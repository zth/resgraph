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
   (= 5.3.0))
  (ocamlformat
   (= 0.28.1))
  (cppo
   (= 1.6.9))
  dune))
