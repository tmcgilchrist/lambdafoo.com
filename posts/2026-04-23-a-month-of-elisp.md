---
layout: post
title: "A month of Elisp"
date: 2026-04-23 14:00
categories: ocaml emacs tree-sitter
---

Somehow this month has turned into a month of Elisp. It's been a while since I posted something Emacs related so here is a roundup of all the things I've done.

## Inspiration

I'm primarily working in OCaml day to day with some C and assembly thrown in for good measure. OCaml has a number of mini-languages it uses for describing things like packages using opam, build rules using dune, LR(1) parser generators using menhir, lexical analysers using ocamllex, integration tests using cram, and so on. Many of these could use a nice tree-sitter mode to power editors like Emacs and NeoVim. Late last year I started looking into the state of tree-sitter grammars for OCaml. There was already a well maintained grammar for the main language but many of the other mini-languages didn't have a good grammar.

Tree-sitter is a parser generator and incremental parsing library used in Emacs (since 29) and NeoVim, and powering [GitHub code navigation and search](https://discuss.ocaml.org/t/ann-code-navigation-and-search-on-github/17999). Increasingly it is a vital part of the tooling for OCaml, so why not contribute here?

## Sitting in trees

I started out with [tree-sitter-dune](https://github.com/tmcgilchrist/tree-sitter-dune), originally by Etienne Millon, and built out support for dune build system files like dune, dune-project and dune-workspace. Covering modern dune since 3.19 and most of the stanzas (the configuration blocks like `(executable ...)` and `(library ...)`), modulo reported issues. The next tree to climb was the menhir grammar.

Menhir is a parser generator that compiles LR(1) grammar specifications into OCaml. It's commonly used by the community and even the [OCaml compiler itself](https://github.com/ocaml/ocaml/blob/trunk/parsing/parser.mly). The existing [tree-sitter-menhir](https://github.com/Kerl13/tree-sitter-menhir) project hasn't seen a release for 3 years and didn't seem active. More importantly it was missing robust support for language injection which we need. Tree-sitter has a concept of injecting another grammar into certain places within the current grammar, commonly used for injecting JavaScript into an HTML grammar. In our case Menhir files contain embedded OCaml code and we want to inject the full OCaml grammar into those places. This gives us full OCaml support without needing to write a second copy of an OCaml grammar inside Menhir, and replaces the ad-hoc regexes used for parsing OCaml code. The bonus is we picked up modern OCaml syntax along the way. The work landed in [PR #8](https://github.com/Kerl13/tree-sitter-menhir/pull/8).

Skipping [ocamllex](https://github.com/314eter/tree-sitter-ocamllex), which was already using language injection and is actively maintained.

Next I moved onto [tree-sitter-opam](https://github.com/tmcgilchrist/tree-sitter-opam) to parse the opam package manager files. This followed a similar pattern to developing tree-sitter-dune, the interesting parts were using the opam-repository for test cases and finding issues with my implementation. Later on I realised the exact structure of the AST (Abstract Syntax Tree) was *much* more important than I thought.

Finally I started an odoc parser. odoc is the OCaml documentation markup language. It's used for both code comments in OCaml source files and standalone .mld files. The focus was on supporting mld files which look similar to markdown files with some extra syntax for referring to OCaml types and code. This time I searched the odoc project for their [parser](https://github.com/ocaml/odoc/tree/master/src/parser) and used their test suite to validate my tree-sitter implementation. There's more work to do here to make it complete and extend support to code comments, perhaps using language injection again.

## Neocaml

Next we need a nice OCaml mode that uses tree-sitter. I've hacked, in the most messy sense, support into [tuareg](https://github.com/ocaml/tuareg) (the long-standing OCaml Emacs mode) but the weight of historical baggage in that code is heavy, so imagine my joy when neocaml was released.

[Neocaml](https://neocaml.org/) started as a modern, tree-sitter-powered Emacs package for programming in OCaml. Version 0.6.0 shipped last month with support for dune and opam files, based off the tree-sitter grammars I'd worked on last year. Now I had users and bug reports and motivation to fix things, which has improved all the grammars with more fixes to come. There's an odoc MLD [PR #46](https://github.com/bbatsov/neocaml/pull/46) coming soon once I sort out font-locking (Emacs-speak for syntax highlighting).

At this point I've moved completely over to neocaml for all my OCaml work. bbatsov has written up the release on [Neocaml 0.6: Opam, Dune, and More](https://batsov.com/articles/2026/03/25/neocaml-0-6-opam-dune-and-more/). He also made a nice [Emacs Prelude 2.0](https://emacsredux.com/blog/2026/03/26/emacs-prelude-redux/) release that uses neocaml. This is the happiest I've been with my Emacs OCaml setup. What is next?

## Combobulate

Building on the shoulders of all this tree-sitter work is combobulate. Combobulate aims to simplify code navigation in Emacs using tree-sitter to provide intuitive, structured navigation that works across languages. Go read [Combobulate: Intuitive, Structured Navigation with Tree-Sitter](https://www.masteringemacs.org/article/combobulate-intuitive-structured-navigation-treesitter) for the full motivation and demo. We recently landed Combobulate support for OCaml in [PR #157](https://github.com/mickeynp/combobulate/pull/157) (with bug fixes in [#159](https://github.com/mickeynp/combobulate/pull/159) and [#161](https://github.com/mickeynp/combobulate/pull/161)). Filled with enthusiasm and the promise of consistent key bindings across all OCaml mini-languages, I threw together [PR #9](https://github.com/tmcgilchrist/combobulate/pull/9) to support dune, opam and odoc files. It needs more polish and day to day use but it is already very good.

On top of that there's some basic Combobulate C support in [PR #11](https://github.com/tmcgilchrist/combobulate/pull/11), and [PR #12](https://github.com/tmcgilchrist/combobulate/pull/12) adds the same for Haskell.

## Curry On!

Having experienced neocaml I've started writing a tree-sitter powered Emacs package for programming in Haskell. The combination of a base tree-sitter mode with a layer of LSP (Language Server Protocol) on top and combobulate on top of that is how language modes should work in Emacs. I'm still furiously hacking on this project and will have more to say when the first version is released.

## Colours and names are hard

I also tidied up and shipped [miasma-theme](https://github.com/tmcgilchrist/miasma-theme), a theme for Emacs that had been sitting in a directory since 2024. It's a dark colour scheme built from muted earth tones (moss, bark, rust, amber, and fog), based on xero's [miasma terminal theme](https://terminalcolors.com/themes/miasma/default/) palette. It fits nicely with my dark theme on macOS. Getting the colours right and covering all the right faces has been a pain; I've got everything I use daily but there are going to be things I missed. The [DESIGN.md](https://github.com/tmcgilchrist/miasma-theme/blob/main/DESIGN.md) goes into the motivation for the colours. Previously I'd just used other people's themes, often the `doom-themes` package or the `solarized-theme` that was default in Emacs prelude.

All up there's been so much Emacs and elisp this month.
