# TextMate grammars

Two kinds of grammar live here, loaded by different rules.

`*.json` in this directory are hand-written for this blog. The filename is the
language id, matched against the fence info string in your Markdown, so
`erlang.json` highlights ```` ```erlang ```` blocks. For a grammar that should
answer to several names, join them with `+`:

    yaml+yml.json        highlights ```yaml and ```yml
    assembly+asm.json    highlights ```assembly and ```asm

The loader rewrites the grammar's own `name` field to each of those ids, which
is how hilite registers its own bundled grammars.

`vendor/*.json` are third-party, kept byte-identical to upstream, and keep their
own `name` and `scopeName` instead. See `vendor/README.md`.

Either way, drop a file in and it is picked up on the next build. No OCaml
changes are needed.

## What is here

| File | Answers to | Notes |
| --- | --- | --- |
| `erlang.json` | `erlang` | module directives, atoms, variables, functions |
| `haskell.json` | `haskell` | `{- -}` and `--` comments, type constructors, signatures |
| `emacs-lisp+common-lisp.json` | `emacs-lisp`, `common-lisp` | one dialect-agnostic Lisp grammar, `:keywords` and `'symbols` |
| `assembly+asm.json` | `assembly`, `asm` | ARM64 and x86 mnemonics, registers, GAS directives, labels |
| `dune.json` | `dune` | generic s-expression rules, overriding hilite's bundled grammar |
| `vendor/c.tmLanguage.json` | `c` | upstream better-c-syntax, MIT |
| `vendor/cpp.tmLanguage.json` | `c++` | upstream better-cpp-syntax, MIT |

Built into hilite already, so not needed here: `ocaml`, `dune`, `opam`,
`diff`, `shell`, `sh`, `bash`.

## Why the rest are hand-written

The obvious source of real grammars is a VS Code install, under
`Contents/Resources/app/extensions/*/syntaxes/`. That is where the two C ones
came from. It bundles about 80 grammars, and none of them is Erlang, Haskell,
Emacs Lisp, Common Lisp, assembly, dune or even OCaml. Those are all
marketplace extensions rather than built-ins, so there is nothing to copy.

The hand-written ones are deliberately small: comments, strings, keywords,
numbers, and the identifiers worth picking out. That covers what blog snippets
need, and is a good deal less than a full editor grammar would do.

## Scope names matter

`../css/syntax-tm.css` matches on scope substrings, so a new grammar only picks
up colour if it uses conventional TextMate scopes. The useful ones are
`comment.*`, `string.*`, `keyword.*`, `constant.numeric.*`,
`entity.name.function.*`, `entity.name.type.*` and `support.type.*`. Remember
that hilite drops the last dot-component, so name scopes
`keyword.control.mylang` rather than plain `keyword.control`.

## What is still unhighlighted

Run `site.exe grammars` from the repository root for the current list.
