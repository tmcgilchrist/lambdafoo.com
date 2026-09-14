---
title: "More tree-sitter, more neocaml, more elisp"
date: 2026-09-14
tags: [ocaml, emacs, tree-sitter, odoc]
description: "Updates on tree-sitter support for OCaml mini-languages and improving Emacs support for OCaml"
---

Back in April I wrote about spending [a month on elisp](/posts/2026-04-23-a-month-of-elisp.html) where I created tree-sitter grammars for the OCaml mini-languages (think dune, opam and odoc) and used them to improve OCaml support in Emacs. The last few weeks I've been working on more of the same. I dug myself out of a hole after the OCaml Workshop and found some time to work on Emacs and elisp. Here's what I worked on.

## Parsing the .mld files people actually write

odoc is the OCaml documentation markup language, used both for comments in source files and for standalone `.mld` pages. The odoc grammar I started in April parsed the odoc I had written in my test corpus. It did not parse the odoc that exists in the wild however. [PR #10](https://github.com/tmcgilchrist/tree-sitter-odoc/pull/10) fixes that, and shipped as [tree-sitter-odoc 0.2.0](https://github.com/tmcgilchrist/tree-sitter-odoc/releases/tag/0.2.0).

The interesting failures were all cases where my grammar was stricter than odoc itself. Ultimately that's my fault for trying to dial in the grammar to be as strict as possible. So what were the issues?

odoc's inline markup can span newlines. odoc turns a newline inside `{b ...}`, a heading or `{{!ref}...}` into a space, so `{b foo\nbar}` is ordinary prose rather than an error. However a blank line still ends it, which I handle with a new external `_inline_newline` token (an external token is one handled by hand-written C in the scanner rather than by the generated parser) so that markup left unclosed while you are still typing does not swallow the rest of the page. This is an interactive fix that you need to experience.

I realised that raw markup is actually an inline element. That sounds like a detail until you notice that `{0 Fmt {%html: ...%}}` is the page title of every dune-released library, and my grammar could not parse it. Which was embarrassing.

Other bug fixes like, light list items run to the next bullet or a blank line, not to the end of the line, so a bullet that wraps is one item. The item, not the list, now carries the newline that separates it from the next one. Light table cells hold inline markup, so `{t | {e emph} | b |}` works.

Code block content ends at its own terminator. The openers `{[`, `{@` and `{delim@` are external tokens now, so the scanner knows which delimiter it is looking for and stops `{delim@lang[...]delim}` at `]delim}` rather than at the first `]}` that happens to appear inside the code. Delimited blocks can also carry the result block that mdx writes back, `{delim@lang[code]delim[output]}`.

Finally, a `{` that opens nothing is literal text. odoc warns and carries on rather than giving up on the rest of the page, and an editor grammar had better do the same. I also fixed the injection query (the rule that tells tree-sitter which region to hand over to another grammar), which was capturing the whole code block, delimiters included, as injected content instead of just the content. These issues were all found by my own use of `neocaml-odoc-mode` with this grammar. The result is a more robust odoc parser that handles more odoc syntax.

## neocaml-odoc-mode

With a more robust grammar that handles the `.mld` files I'm writing, [PR #46](https://github.com/bbatsov/neocaml/pull/46) is finally in reasonable shape. It has been open since April, mostly waiting on me sorting out font-locking (Emacs-speak for syntax highlighting), and has now been merged and hopefully officially released soon.

`neocaml-odoc-mode` is a tree-sitter major mode for `.mld` documentation pages. Font-lock support covers headings, inline markup, code spans and blocks, references, links, tags, lists and tables, each with a dedicated `neocaml-odoc-*` face (a face being the bundle of colour and weight Emacs paints text with) so you get colours by default with the option to customise things as much as you want. I also added indentation, imenu (the buffer index you navigate with) and heading-based defun navigation, so `C-M-a` and `C-M-e` step between sections which I use frequently with Combobulate. You can install the grammar with `M-x neocaml-odoc-install-grammar`. The work follows the existing style of the neocaml package, so it should be an easy thing to adopt.

The part I like most is language injection. On Emacs 30+, an `{@ocaml[...]}` block gets full OCaml highlighting, and the same goes for dune and opam blocks, using the tree-sitter grammars from my earlier work. Everything else falls back to a plain code face, but highlighting could be added if someone wanted to do the work for other languages. Having documentation that highlights its own code examples with the same machinery as the source files seems like a small thing, but it is the sort of polish that makes a mode feel finished.

Now you get a nice view like:

![neocaml-odoc-mode highlighting a .mld page](/images/neocaml-odoc-syntax.png)

## A segfault, and how to avoid one

Emacs 31.1 has been out for a while. I'm usually conservative with updating core software like Emacs and macOS versions, but now it was time to update things, experience the new features and see what breaks. A quick `brew upgrade --cask emacs` and now Emacs was successfully SEGV'ing like you don't want it to.

I added Emacs 31.1 to the neocaml CI matrix in [PR #80](https://github.com/bbatsov/neocaml/pull/80), just to confirm it wasn't just me. Good and bad, Emacs promptly started crashing on any OCaml buffer containing a type constructor. A simple expression like `let x : int = 42` was enough.

The cause is a nice bit of accidental interaction in Emacs's tree-sitter support. `regexp-opt` with `symbols` wraps its alternation in `\_< ... \_>`. Evaluating a symbol-boundary assertion inside a `:match` predicate pulls in lazy `syntax-propertize`, and it does that while `treesit-query-capture` is still walking the query cursor. Emacs 31.1 does not survive the re-entry and we SEGV our editor. So firing up Emacs from LLDB was the next step and getting a backtrace for the offending code. A few moments later ... and much code reading, I had an idea and a workaround. The underlying Emacs bug is [#81729](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=81729) and had already been reported by Brian Ward.

Meanwhile, the fix in [PR #79](https://github.com/bbatsov/neocaml/pull/79) is to anchor the match to the node text instead. The captures are whole `value_name`, `constructor_name` and `type_constructor` nodes and both builtin lists are plain identifiers, so `` \`...\' `` is exactly equivalent to symbol boundaries here. That side-steps the crash without weakening the feature.

Also [#80](https://github.com/bbatsov/neocaml/pull/80) switched to using the hybrid `forward-sexp` function on every Emacs version, so `C-M-f` moves over a whole keyword-led form (`fun`, `match`, `if`) on Emacs 31+ instead of stopping after the keyword.

## Flattening the opam tree

In April I noted that the exact shape of the AST (Abstract Syntax Tree) turned out to be *much* more important than I expected for tree-sitter libraries. In tree-sitter-opam, `value` was a pure grouping choice that wrapped every value in a redundant node. Renaming it `_value` makes it a hidden rule (a leading underscore is tree-sitter's convention for hiding one), which promotes its alternatives into the enclosing node, and `queries/tags.scm` moves to matching the concrete nodes instead.

It's a breaking change as `value` is no longer a node type, and it churned every test corpus file in the repository. It also makes every query written against the grammar shorter and every navigation command behave the way you would expect, which is the whole point. Grammars are an interface, and an interface that leaks its own grouping decisions is a bad one. I'm testing this out locally before I push out a release of `tree-sitter-opam`.

## tree-sitter-dune odds and ends

Marek Kubica has been doing good work on tree-sitter-dune while I was elsewhere. [PR #19](https://github.com/tmcgilchrist/tree-sitter-dune/pull/19) adds the `tags` stanza (stanzas are the configuration blocks like `(executable ...)` and `(library ...)` that a dune file is built from), and [PR #21](https://github.com/tmcgilchrist/tree-sitter-dune/pull/21) adds pforms (`%{...}`, dune's percent forms for variable substitution) as bare atoms, with the harder case of pforms inside strings left for later. There is an open naming question there, `variable` for bare atoms against `interpolation` for the in-string case, and the tree-sitter [standard rule names](https://tree-sitter.github.io/tree-sitter/creating-parsers/3-writing-the-grammar.html#standard-rule-names) do not settle it.

On my side I have a branch adding a `stanza_field` wrapper node for field-value pairs, extended across all the remaining stanza fields. It is the opposite move to the opam one above, adding structure rather than removing it, because here the field and its value really are a unit that queries want to grab together.

Unlike the other grammars, dune keeps changing and adding new features so the tree-sitter support needs more frequent attention. My use of dune is fairly simple and I'm not using the new features or triggering these edge cases. It feels like there is an opportunity for tooling to do eager integration of dune trunk.

## Neocaml DAP support

Slightly off to the side, [PR #81](https://github.com/bbatsov/neocaml/pull/81) adds a dape setup for native debugging of OCaml, with a `docs/debugging.md` to go with it. dape is an Emacs client for the Debug Adapter Protocol (DAP), the wire protocol between an editor and a debug server like LLDB or GDB. Back in [Debugging OCaml with Emacs](/posts/2024-03-25-ocaml-debugging-with-emacs.html) I wrote up a similar setup using dap-mode. Since then I haven't used the editor integration for debugging that much, so I missed the addition of dape bytecode debugging instructions to neocaml. This rounds out the debugger tooling story for neocaml with the rest of the work needing to be done in LLDB/GDB or OCaml itself. The OCaml manual documents the current state of native and bytecode debugging, and OxCaml has an even better story for native debugging support that I want to cover soon.

## Future Work

What's up next in Emacs for OCaml?

I have an old demo from June that adds runnables support to ocamllsp. The goal is parity with the rust-analyzer experience. Imagine the user invokes a command at point or per-file, gets a list of things that can be run (executables, test stanzas, individual inline tests, cram files), and the editor executes the chosen item, for example debug, run, or copy as shell command. The other idea I've been working on since the OCaml Workshop is adding texi output to odoc 3. The idea is to generate GNU Info manuals for every package installed in a local opam switch, as an alternative to odoc's HTML or Markdown output, and make them reachable from Emacs with `C-h i` and from a symbol at point. Finally I have a branch for Combobulate that adds basic navigation for odoc, dune and opam files, that needs rebasing and testing against the Combobulate OCaml support that already exists.
