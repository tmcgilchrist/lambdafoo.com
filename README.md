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
./_build/default/main/site.exe --help       # the full command list
./_build/default/main/site.exe build        # build into _site/
./_build/default/main/site.exe serve        # build and serve on :8000
./_build/default/main/site.exe serve 8080   # ...or on another port
./_build/default/main/site.exe grammars     # syntax highlighting coverage
```

The CLI is [cmdliner](https://erratique.ch/software/cmdliner)-based, so every
subcommand takes `--help` and gets its own man page. Running it with no
arguments prints the command list rather than doing anything.

Rebuilds are content-hashed, so an edit to one post rewrites one file. Delete
`_site/` for a full rebuild.

### Shell completion

Subcommands, options and draft filenames all complete. cmdliner ships the
generic completion scripts, so the shell only needs to be told that `site`
uses them. For zsh, with the switch active:

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
./_build/default/main/site.exe serve --drafts 8080
./_build/default/main/site.exe build --drafts       # build only
```

Dev mode writes to `_site_dev/`, not `_site/`, so a draft cannot reach the
deployed output even if you forget which mode you last built in. Everything
else about the two trees is identical: `_site_dev` is `_site` plus the draft
pages. CI passes no flag, and `drafts/` is untracked anyway, so it never
reaches the runner.

Drafts are read with a lenient version of the post archetype: no `title:` and
no front matter at all are both fine, since several drafts are in that state.
A draft with no title falls back to its filename, and one with no date renders
without a date line rather than as 1970.

``` shell
./_build/default/main/site.exe check-drafts
./_build/default/main/site.exe check-drafts oxcaml-yaks.md   # just one
./_build/default/main/site.exe new-draft "On DWARF and OCaml"
```

`check-drafts` reports which drafts could move to `posts/` as they stand. Two
things block that: missing front matter or `title:`, and a filename with no
`YYYY-MM-DD` prefix, because the filename date is what a post publishes under
(see the note on dates below). Missing tags are reported but do not block.

`new-draft` scaffolds `drafts/YYYY-MM-DD-slug.md` with front matter that
passes the check, dated today.

## Layout

| Path                 | Purpose                                                |
|----------------------|--------------------------------------------------------|
| `main/site.ml`       | The generator: archetypes, tasks and actions           |
| `main/redirects.ml`  | 60 legacy `/blog/YYYY/MM/DD/slug/` redirects           |
| `drafts/`            | Unpublished drafts, dev mode only                      |
| `main/grammars/`     | Extra TextMate grammars for syntax highlighting        |
| `templates/`         | Jingoo templates                                       |
| `posts/`, `pages/`   | Content                                                |
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

This site was generated by Hakyll until August 2026, and the port is
deliberately faithful. Every URL the Hakyll build produced is still produced,
including all 60 redirects, and `main/site.ml` reproduces some Hakyll quirks on
purpose. The notable one is dates: Hakyll's `getItemUTC` rejects the
`YYYY-MM-DD HH:MM` form used in most of the front matter here and silently falls
back to the date in the filename, which is what actually determines post order
on the live site.

Known rendering differences, none of which change a URL:

  * CommonMark has no smart typography, so quotes and dashes stay ASCII.
  * No implicit figures, no `@`-citations, no `<ol type="1">`.
  * URLs are root-relative (`/css/…`) rather than relativised (`../css/…`).
  * CSS is copied rather than minified.
  * Feed bodies are entity-escaped rather than wrapped in CDATA.
  * Headings carry generated anchor ids, and code blocks no longer carry
    per-line anchors.
