# Vendored TextMate grammars

Third-party grammars, copied here byte for byte. Nothing in this directory is
edited, so `shasum -a 256` against the upstream file is the whole audit.

Unlike `../*.json`, these are registered under the grammar's own `name`
(lowercased by the registry) and its own `scopeName`, not under the filename.
That keeps the files diffable. Where upstream's `name` is not the fence label,
`aliases.json` maps the label to the file, which is also how `c++` gets a name
at all, since the `+` in the sibling filename convention cannot spell it.

| File | Answers to | Upstream | Licence |
| --- | --- | --- | --- |
| `c.tmLanguage.json` | `c` | [better-c-syntax](https://github.com/jeff-hykin/better-c-syntax) @ `34712a61` | MIT |
| `cpp.tmLanguage.json` | `c++` | [better-cpp-syntax](https://github.com/jeff-hykin/better-cpp-syntax) @ `071dd6ec` | MIT |
| `JSON.tmLanguage.json` | `json` (via alias) | [vscode-JSON.tmLanguage](https://github.com/microsoft/vscode-JSON.tmLanguage) @ `9bd83f1c` | MIT |
| `coffeescript.tmLanguage.json` | `coffeescript` | [atom/language-coffee-script](https://github.com/atom/language-coffee-script) @ `0f6db914` | MIT |
| `ruby.tmLanguage.json` | `ruby` | [textmate/ruby.tmbundle](https://github.com/textmate/ruby.tmbundle) @ `efcb8941` | TextMate Bundle |
| `yaml.tmLanguage.json` | `yaml`, `yml` (via alias) | [textmate/yaml.tmbundle](https://github.com/textmate/yaml.tmbundle) @ `e54ceae3` | TextMate Bundle |

See `LICENSE`. Each file records its own upstream commit in a `version` field,
so provenance travels with the file.

    sha256  a4a720d99871e742a021a99f52d5bfd954f64388a45b46d80a29b58fdec1ad6a  c.tmLanguage.json
    sha256  5eeac5e63c17589c5cfd4df6e9bd86c4bdbcbabc54460b9c3457815dd52e63ff  cpp.tmLanguage.json
    sha256  e0f398297881c13909fa5d30e7e5e7632defabb4e8d60dfd5ba94d0bf09e8898  JSON.tmLanguage.json
    sha256  92ce288076a28d53b9b74fbb8ca52b01086a2ef215f7782d8658463b54144e45  coffeescript.tmLanguage.json
    sha256  241a8d95350746cff04219962567bc8875e23db9b4e08f0ab0b4d37f640713e1  ruby.tmLanguage.json
    sha256  0e9e3669a65b5d1f957398844f50ee6e51e713f5a18f782ec458cdfbaebf1bb6  yaml.tmLanguage.json

## Where they came from

C, C++, JSON and CoffeeScript were copied out of a local VS Code install:

    /Applications/Visual Studio Code.app/Contents/Resources/app/extensions/*/syntaxes/

Ruby and YAML come from VS Code 1.60.0 rather than current, because the
grammars VS Code ships today do not work here. Both were fetched from

    https://raw.githubusercontent.com/microsoft/vscode/1.60.0/extensions/<lang>/syntaxes/

## Grammars that had to be rejected

`textmate-language` is stricter than the engine VS Code uses, and three current
upstream grammars fail against it. This is recorded so nobody re-tries them.

  * **Ruby**, current (Shopify/ruby-lsp). Loads, but tokenising a real post
    trips `assert (j > i)` in hilite, a zero-width token, which aborts the whole
    build partway through. The older textmate/ruby.tmbundle grammar is used
    instead.
  * **YAML**, current (RedCMD/YAML-Syntax-Highlighter). The `yaml.tmLanguage.json`
    shim is rejected with "patterns not found", because its `repository.parity`
    entry holds only a comment and no rules. The `yaml-1.2.tmLanguage.json` it
    delegates to is rejected with "Type error: Expected dict", because a
    `beginCaptures` entry carries a `comment` string where a capture object is
    expected. The older textmate/yaml.tmbundle grammar is used instead.
  * **XML**, both current and 1.60.0 (atom/language-xml). Rejected with "Begin
    patterns must either have an end or while". The `<%-- --%>` comment rule has
    its `end` and `name` nested inside `captures` by mistake, an upstream typo
    that VS Code tolerates by dropping the rule. There is a hand-written
    `../xml.json` instead.

`cpp.embedded.macro.tmLanguage.json` (294 KB) sits alongside the C++ grammar
upstream and gives multi-line `#define` bodies their own tokenisation. It is not
here: it changes nothing for this blog, verified by diffing the rendered posts,
and most of its rules are cross-grammar repository includes of the form
`source.cpp#some_rule`, which `textmate-language` does not resolve.

## Two things to know about the rendered output

Grammars include each other for embedded languages: Ruby alone reaches for
fourteen other scopes, and the C grammars reach for `source.arm` and `source.x86`
for inline assembly. Those are not here, and an unresolvable scope include is
skipped rather than raising, so embedded snippets fall back to plain text.

better-cpp-syntax names some scopes with capture backreferences, such as
`keyword.control.directive.$5.c`. TextMate substitutes the captured text there.
`textmate-language` does not, so a few emitted classes carry a literal `$5`.
`../../../css/syntax-tm.css` matches on substrings, so they still take colour.
