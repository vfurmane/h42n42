(lang dune 3.16)

(name h42n42)

(generate_opam_files true)

(source
 (github vfurmane/h42n42))

(authors "Valentin Furmanek")

(maintainers "Valentin Furmanek")

(license MIT)

(documentation https://vfurmane.github.io/h42n42)

(package
 (name h42n42)
 (synopsis "Simulate the life of a population of creatures threatened by a terrifying virus")
 (description "Introduction project to the OCSIGEN Framework, allowing one to create rich web applications written in OCaml. Conceive a simulator where Creature must survive to a deadly virus.")
 (depends
  ocaml
  dune
  ocamlformat
  odoc
  (ounit2 :with-test)
  bisect_ppx
  ocsigenserver)
 (tags
  (topics "school project")))
