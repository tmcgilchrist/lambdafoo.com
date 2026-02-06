---
layout: post
title: "Getting Started with OCaml in 2021"
date: 2021-09-28 12:48
comments: false
categories: ocaml
---

OCaml is an awesome language with many fine features. I enjoy using it immensely!

Unfortunately, it suffers from a perceived weakness in how to get started. Like any new skill, there can be a learning curve. The tools are all there, but combining them for a good developer experience might seem difficult at first.

Often I've found that the barrier for getting into a new langauge is less about the
new features of that language and more about learning the tools to become productive in that
language. The package managers, build tools, and editor integration of a new language can be confusing, making for an awful experience.

Perhaps my opinionated guide to getting started with OCaml in 2021 will help reduce any mental blocks against trying out this excellent language.

Install Opam
----------

First it's necessary to install OCaml and Opam.
[Opam][] is the default package manager for OCaml projects.
Ignore the other options for now, once you know more about what you want, you can make
an informed choice. For now if you speak OPAM, you'll get the most out of the community.

On Linux, use your local package manger, e.g., `apt-get install opam` for Debian and `apt install opam`
for Ubuntu. For MacOS, use homebrew `brew install opam`. I'll assume if you run
something else, you can handle looking up [how to install things][].

On my Mac I get Opam 2.1.0:

``` shell
$ opam --version
2.1.0
```

Once you've got Opam installed, you should be able to move on to the next step.

Choose an OCaml Version
----------

I strongly recommended that you pick a single OCaml version that your project will compile against.
Supporting multiple compiler versions is possible and usually not too diffcult, but it complicates
the process right now.

Running `opam switch list-available` will show you a long list of every possible OCaml compiler.
Choose the latest mainline compiler identifed by `Official release X.XX.X` where currently the latest
is `4.13.0`. Ignore the others.

``` shell
opam switch list-available
...
ocaml-variants                         4.12.0+domains                         OCaml 4.12.0, with support for multicore domains
ocaml-variants                         4.12.0+domains+effects                 OCaml 4.12.0, with support for multicore domains and effects
ocaml-variants                         4.12.0+options                         Official release of OCaml 4.12.0
ocaml-base-compiler                    4.12.1                                 Official release 4.12.1
ocaml-variants                         4.12.1+options                         Official release of OCaml 4.12.1
ocaml-variants                         4.12.2+trunk                           Latest 4.12 development
ocaml-base-compiler                    4.13.0~alpha1                          First alpha release of OCaml 4.13.0
ocaml-variants                         4.13.0~alpha1+options                  First alpha release of OCaml 4.13.0
ocaml-base-compiler                    4.13.0~alpha2                          Second alpha release of OCaml 4.13.0
ocaml-variants                         4.13.0~alpha2+options                  Second alpha release of OCaml 4.13.0
ocaml-base-compiler                    4.13.0~beta1                           First beta release of OCaml 4.13.0
ocaml-variants                         4.13.0~beta1+options                   First beta release of OCaml 4.13.0
ocaml-base-compiler                    4.13.0~rc1                             First release candidate of OCaml 4.13.0
ocaml-variants                         4.13.0~rc1+options                     First release candidate of OCaml 4.13.0
ocaml-base-compiler                    4.13.0~rc2                             Second release candidate of OCaml 4.13.0
ocaml-variants                         4.13.0~rc2+options                     Second release candidate of OCaml 4.13.0
ocaml-base-compiler                    4.13.0                                 Official release 4.13.0
ocaml-variants                         4.13.0+options                         Official release of OCaml 4.13.0
ocaml-variants                         4.13.1+trunk                           Latest 4.13 developmet
ocaml-variants                         4.14.0+trunk                           Current trunk
...
```

At this point, install the latest OCaml 4.13.0:

``` shell
$ opam switch create 4.13.0

<><> Installing new switch packages <><><><><><><><><><><><><><><><><><><><>  🐫
Switch invariant: ["ocaml-base-compiler" {= "4.13.0"} | "ocaml-system" {= "4.13.0"}]

<><> Processing actions <><><><><><><><><><><><><><><><><><><><><><><><><><>  🐫
∗ installed base-bigarray.base
∗ installed base-threads.base
∗ installed base-unix.base
∗ installed ocaml-options-vanilla.1
⬇ retrieved ocaml-base-compiler.4.13.0  (https://opam.ocaml.org/cache)
∗ installed ocaml-base-compiler.4.13.0
∗ installed ocaml-config.2
∗ installed ocaml.4.13.0
Done.
```

You can start using this version by typing the following:

``` shell
$ opam switch set 4.13.0
```

And verify which switch you are using:
``` shell
$ opam switch show
4.13.0
```

When you work with several OCaml projects, it's best to create a switch per project, as it keeps
each project isolated and prevents issues with installing conflicting versions of libraries.
For example, I use a naming scheme of `ocaml-version-project-name`,
e.g., `4.13.0-ocurrent`. Then in each project directory, run `opam switch link 4.13.0-ocurrent`
to setup that named switch for that specific directory. Opam will take care of setting that switch
in your shell when you change into that directory.

Creating Your Project Directory
----------

For this step we need the [Dune][] build tool, so go ahead and install it with `opam install dune`.
Dune comes with a simple scaffolding command to create an empty project that is really useful
to get started.

I'm calling my project `box`, so run:

``` shell
$ dune init proj box
Success: initialized project component named box
```

In the project generated, we get a library component, a CLI, and a test component, which will
all compile out of the box.

``` shell
$ cd box
$ tree
.
├── bin
│   ├── dune
│   └── main.ml
├── box.opam
├── lib
│   └── dune
└── test
    ├── box.ml
    └── dune

3 directories, 6 files
```

Lets try a compile:

``` shell
$ dune build @all
Info: Creating file dune-project with this contents:
| (lang dune 2.8)
| (name box)

```

Running the CLI:

``` shell
$ dune exec bin/main.exe
Hello, World!
```

Each of the `bin`, `lib`, and `test` directories contains the source code in the form of `*.ml` files,
along with a `dune` file which tells Dune how to build the source and on what libraries it depends.
The box `bin\dune` file declares it's an `executable` with a name `box` and depends on the `box`
library.

``` shell
(executable
 (public_name box)
 (name main)
 (libraries box))
```

Adding a Dependency
----------

CLI tools require command line parsing, `Cmdliner` is a common library that implements CLI parsing.
We need to add it in two places: first in the `dune-project` file, to get it installed, and then
in `bin/dune`, to say where we're using it.

One small digression, when generating our project, `dune` created an `box.opam` file. This describes
our project to Opam, telling it what libraries it requires and what the project does. You need this
if you ever publish a package for other people to use. Newer versions of Dune can generate the `box.opam `
file from a `dune-project` file. Having a single source of information is helpful, so lets create that file:

``` shell
(lang dune 2.8)
(name box)

(generate_opam_files true)

(package
 (name box)
 (depends
  (ocaml (>= 4.13.0))
  (cmdliner (>= 0.9.8)))
 (synopsis "Box cli"))
```

Remove the `rm box.opam` file to test the generation. Now run `dune build @all` to regenerate the Opam
file. This file should be checked in, and any further edits should be at the top-level `dune-project`
file, which should look like this:

``` shell
$ cat box.opam
# This file is generated by dune, edit dune-project instead
opam-version: "2.0"
synopsis: "Box cli"
depends: [
  "dune" {>= "2.8"}
  "ocaml" {>= "4.13.0"}
  "cmdliner" {>= "0.9.8"}
  "odoc" {with-doc}
]
build: [
  ["dune" "subst"] {dev}
  [
    "dune"
    "build"
    "-p"
    name
    "-j"
    jobs
    "@install"
    "@runtest" {with-test}
    "@doc" {with-doc}
  ]
]

```

The final step is to actually install the `cmdliner` library. Run `opam install . --deps-only -ty`,
which will look at the `*.opam` files present and install just their dependencies with the correct
version bounds.
The `-y` says yes to installing the packages. You can remove it if you like by pressing `Y` or if
you want to review what will be installed.
`-t` will run the package tests, which isn't always necessary, but it's sometimes useful for certain
packages with native C components.

Alternatively you could run `opam install cmdliner`,  as this doesn't look at version constraints in `*.opam` files, you might not get what you expect.

Editor Tooling
----------

Finally, you'll want to get comfy with your chosen editor. If you have a preference, you should use the native LSP support in that editor, along with installing `opam install ocaml-lsp-server`. OCaml is standardising on the LSP protocol for editor interaction. If you have no editor preference, then start with [VSCode][] and install the OCaml LSP package from the Marketplace.

Personally, I'm using Emacs with the LSP mode `eglot`, which works really nicely, along with some customisations to bind certain LSP actions to keys. I highly recommend getting into Emacs as an editor because the customisation via a fully-featured language, like Lisp, is fantastic if you live in your editor like I do.

This post is an update to an earlier post by [Adam][] in 2017, and I hope this short tutorial helps get you started with OCaml!

[OPAM]: https://opam.ocaml.org
[how to install things]: https://opam.ocaml.org/doc/Install.html#Using-your-distribution-39-s-package-system
[VSCode]: https://code.visualstudio.com
[Adam]: https://adambard.com/blog/getting-started-with-ocaml/
[Dune]: https://dune.readthedocs.io