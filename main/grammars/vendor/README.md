# Vendored TextMate grammars

Third-party grammars, copied here byte for byte. Nothing in this directory is
edited, so `shasum -a 256` against the upstream file is the whole audit.

Unlike `../*.json`, these are registered under the grammar's own `name`
(lowercased by the registry) and its own `scopeName`, not under the filename.
That is deliberate: it keeps the files diffable, and `c++` cannot be spelled
with the `+` alias convention the hand-written grammars use.

| File | Registers as | Upstream | Licence |
| --- | --- | --- | --- |
| `c.tmLanguage.json` | `c` | [better-c-syntax](https://github.com/jeff-hykin/better-c-syntax) @ `34712a61` | MIT |
| `cpp.tmLanguage.json` | `c++` | [better-cpp-syntax](https://github.com/jeff-hykin/better-cpp-syntax) @ `071dd6ec` | MIT |

Both are MIT, copyright Jeff Hykin. See `LICENSE`.

## Where they came from

Copied out of a local VS Code install:

    /Applications/Visual Studio Code.app/Contents/Resources/app/extensions/cpp/syntaxes/

VS Code converts them from the upstream repositories and records the source
commit in each file's own `version` field, so provenance travels with the file.
The licences are recorded upstream in `extensions/cpp/cgmanifest.json` of
`microsoft/vscode`.

    sha256  a4a720d99871e742a021a99f52d5bfd954f64388a45b46d80a29b58fdec1ad6a  c.tmLanguage.json
    sha256  5eeac5e63c17589c5cfd4df6e9bd86c4bdbcbabc54460b9c3457815dd52e63ff  cpp.tmLanguage.json

## What was left behind

`cpp.embedded.macro.tmLanguage.json` (294 KB) sits alongside these upstream and
gives multi-line `#define` bodies their own tokenisation. It is not here for two
reasons. It changes nothing for this blog, verified by diffing the rendered
posts with and without it, and most of its rules are cross-grammar repository
includes of the form `source.cpp#some_rule`, which `textmate-language` does not
resolve. It only understands `#local` includes and whole-grammar `source.x`
includes. Copy it in if a post ever needs it.

`platform.tmLanguage.json` is an injection grammar for platform-specific
macros, and `cuda-cpp.tmLanguage.json` is CUDA. Neither is relevant here.

## Two things to know about the rendered output

`source.arm`, `source.x86` and `source.asm` are included by these grammars for
inline assembly. Those grammars are not here, and an unresolvable scope include
is skipped rather than raising, so inline `asm` blocks fall back to plain C.

better-cpp-syntax names some scopes with capture backreferences, such as
`keyword.control.directive.$5.c`. TextMate substitutes the captured text there.
`textmate-language` does not, so a few emitted classes carry a literal `$5`.
`../../css/syntax-tm.css` matches on substrings, so they still take colour, but
that is where the odd-looking class names in the C output come from.
