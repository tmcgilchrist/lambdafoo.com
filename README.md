lambdafoo.com
==========

Tim McGilchrist's blog, generated with [YOCaml 3](https://github.com/xhtmlboi/yocaml).

## Building

YOCaml needs OCaml >= 5.1.1.

``` shell
opam switch create . 5.4.1        # or reuse an existing 5.x switch
opam install . --deps-only -y
dune build
```

## Running

Every command is run **from the repository root**, because the generator
reads `posts/`, `pages/`, `templates/`, `css/`, `images/`, `talks/` and
`main/grammars/` relative to the working directory.

``` shell
dune exec main/site.exe -- --help       # the full command list
dune exec main/site.exe -- build        # build into _site/
dune exec main/site.exe -- serve        # build and serve on :8000
dune exec main/site.exe -- serve 8080   # ...or on another port
dune exec main/site.exe -- grammars     # syntax highlighting coverage
```

The `--` is needed, otherwise dune reads the flags as its own. `dune exec`
rebuilds the generator first, so when you are iterating on content rather than
on the generator, `./_build/default/main/site.exe build` skips that step and is
a little quicker. The two are equivalent.

The CLI is [cmdliner](https://erratique.ch/software/cmdliner)-based, so every
subcommand takes `--help` and gets its own man page. Running it with no
arguments prints the command list rather than doing anything.

Rebuilds are content-hashed, so an edit to one post rewrites one file. Delete
`_site/` for a full rebuild.

### Shell completion

Subcommands, options and draft filenames all complete. This works when you
invoke the binary directly, not through `dune exec`, since in that form the
shell is completing `dune`'s own arguments.

cmdliner ships the generic completion scripts, so the shell only needs to be
told that `site.exe` uses them. For zsh, with the switch active:

``` shell
FPATH="$(opam var share)/zsh/site-functions:${FPATH}"
autoload -Uz compinit && compinit -u
autoload _cmdliner_generic && compdef _cmdliner_generic site.exe
```

`cmdliner generic-completion` prints the scripts for other shells, and
`cmdliner install tool-support` installs a definition for a tool that is on
`PATH`. Neither applies here while the binary is run from `_build/`.

## Drafts

Drafts live in `drafts/` and are never part of the deployed site. `--drafts`
turns on dev mode, which renders them at `/drafts/<name>.html` with an index at
`/drafts.html`, and adds a Drafts link to the sidebar.

``` shell
dune exec main/site.exe -- serve --drafts 8080
dune exec main/site.exe -- build --drafts       # build only
```

Dev mode writes to `_site_dev/`, not `_site/`, so a draft cannot reach the
deployed output even if you forget which mode you last built in. Everything
else about the two trees is identical: `_site_dev` is `_site` plus the draft
pages. `drafts/` is gitignored, so nothing in it is committed and CI never
sees it at all.

### The shape of a draft, and of a post

A draft is named `drafts/<slug>.md`, with no date in the filename, and carries
all three front matter fields. Only the title needs a value:

``` yaml
---
title: "On DWARF and OCaml"
date:
tags:
description:
---
```

Leaving `date:` and `tags:` empty is expected while a piece is still being
written. A draft with no date renders without a date line rather than as 1970.

Publishing means moving the file to `posts/`, adding a `YYYY-MM-DD` prefix to
the filename, and filling the fields in. In `posts/` the metadata must be
valid: every post carries a `title:`, a bare `YYYY-MM-DD` `date:` equal to its
filename prefix, and a `tags:` list.

``` shell
dune exec main/site.exe -- new-draft "On DWARF and OCaml"
dune exec main/site.exe -- check-drafts
dune exec main/site.exe -- check-drafts oxcaml-yaks.md   # just one
```

`new-draft` scaffolds a draft in that shape. `check-drafts` reports any that
have drifted from it: missing front matter, a missing `title:`, a missing
`date:` or `tags:` key, or a filename that has picked up a date prefix. Empty
`date:` and `tags:` values are reported but do not count as problems.

## Layout

| Path                        | Purpose                                         |
|-----------------------------|-------------------------------------------------|
| `main/site.ml`              | The generator: archetypes, tasks and actions    |
| `main/redirects.ml`         | 60 legacy `/blog/YYYY/MM/DD/slug/` redirects    |
| `drafts/`                   | Unpublished drafts, dev mode only               |
| `main/grammars/`            | Extra TextMate grammars for syntax highlighting |
| `templates/`                | Jingoo templates                                |
| `posts/`, `pages/`          | Content                                         |
| `css/`, `images/`, `talks/` | Assets, copied through unchanged                |

## Deployment

`.github/workflows/ocaml.yml` builds the site on every push to `master` and
publishes `_site/` to the `gh-pages` branch.

## Syntax highlighting

Highlighting happens at build time via
[hilite](https://github.com/patricoferris/hilite) and TextMate grammars, and it
has two halves. Both must be present for anything to appear.

**Colours.** hilite names each span `<lang>-<textmate scope minus its last
component>`, so `keyword.other.ocaml` becomes `ocaml-keyword-other`. Those names
share nothing with the pandoc/skylighting classes that `css/syntax.css` knows
about, so `css/syntax-tm.css` supplies the rules. It matches on scope
substrings, so one rule covers every language.

**Grammars.** hilite bundles only `ocaml`, `dune`, `opam`, `diff` and
`shell`/`sh`/`bash`. For anything else, drop a TextMate grammar into
`main/grammars/` and it is picked up on the next build with no code change. See
`main/grammars/README.md`.

## Notes on the port from Hakyll

This site was generated by Hakyll until August 2026. Every URL the Hakyll build
produced is still produced, including all 60 redirects.

One Hakyll quirk is worth knowing about, because it shaped the content. Hakyll's
`getItemUTC` rejected the `YYYY-MM-DD HH:MM` form that most of the front matter
used, and silently fell back to the date in the filename, so for years the
filename was what actually determined post dates and ordering. The generator
reproduced that faithfully at first. Rather than keep modelling it, every post
in `posts/` now carries a bare `YYYY-MM-DD` equal to its filename prefix, and
the generator simply reads the field. Keep it that way: `check-drafts` warns
when a draft breaks the invariant.

That migration corrected one post. `2011-10-06-ipad-and-ebooks-galore` had
`date: 2011-01-06`, a bare date Hakyll accepted, so it published as January 2011
under an October URL. It is now October 2011. No URL changed.

Known rendering differences, none of which change a URL:

  * CommonMark has no smart typography, so quotes and dashes stay ASCII.
  * No implicit figures, no `@`-citations, no `<ol type="1">`.
  * URLs are root-relative (`/css/…`) rather than relativised (`../css/…`).
  * CSS is copied rather than minified.
  * Feed bodies are entity-escaped rather than wrapped in CDATA.
  * Headings carry generated anchor ids, and code blocks no longer carry
    per-line anchors.
