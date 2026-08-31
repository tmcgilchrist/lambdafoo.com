(lang dune 3.18)

(name lambdafoo)

(generate_opam_files true)

(source (github tmcgilchrist/lambdafoo.com))
(license BSD-3-Clause)
(authors "Tim McGilchrist <timmcgil@gmail.com>")
(maintainers "Tim McGilchrist <timmcgil@gmail.com>")

(package
 (name lambdafoo)
 (allow_empty)
 (synopsis "lambdafoo.com, a static site generated with YOCaml")
 (description
  "The generator behind lambdafoo.com. Reads posts/, pages/, templates/ and \
   the assets beside them, and writes a complete site into _site/.")
 (depends
  (ocaml (>= 5.1.1))
  (yocaml (>= 3.0.0))
  (yocaml_unix (>= 3.0.0))
  (yocaml_yaml (>= 3.0.0))
  (yocaml_jingoo (>= 3.0.0))
  (yocaml_markdown (>= 3.0.0))
  (yocaml_syndication (>= 3.0.0))
  (hilite (>= 0.5.0))
  textmate-language
  (cmdliner (>= 2.0.0))
  yojson))
