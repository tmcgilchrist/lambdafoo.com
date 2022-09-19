---
layout: post
title: "OCaml with Emacs in 2022"
date: 2022-09-08 11:33
comments: false
categories: ocaml, emacs
published: true
---

I am revisiting my [OCaml setup post from 2021](https://lambdafoo.com/posts/2021-10-29-getting-started-with-ocaml.html) because I needed to setup a new macOS machine. The official OCaml site points newcomers to [Visual Studio Code](https://ocaml.org/docs/up-and-running#editor-support-for-ocaml) which is a fine choice to get started. However I am using [Emacs](https://www.gnu.org/s/emacs/) and have done so for over 20 years, and did not find a good description of how to set things up with Emacs. Here I could digress into why Emacs but I will just strongly encourage any developers to invest heavily in learning their editor with Emacs being a fine choice.

Beginnings
----------

On macOS I use the pre-compiled GUI version of Emacs from [emacsformacosx](https://emacsformacosx.com) preferring that over compiling it by hand or using the version in [homebrew](https://brew.sh). Both of which I have done previously but find the emacsformacos version saves me time and effort, plus the GUI version was removed from homebrew at some point in the past.

Next I choose to use an Emacs distro over the base Emacs setup, again this is a time saving choice and especially useful if you are new to Emacs. Use [Prelude](https://github.com/bbatsov/prelude), which is an enhanced Emacs 25.1+ distribution that should make your experience with Emacs both more pleasant and more powerful. It gives a great modern setup for Emacs with minimal fuss. Once that is cloned and installed the Lisp config begins.

Prelude onfiguration
----------

Prelude provides a base experience of packages available with some configuration. The configuration goes into `~/.emacs.d/tsmc/prelude-modules.el` where `tsmc` is your macOS user. The same path would apply for Linux. A sample prelude-modules.ml is provided in https://github.com/bbatsov/prelude/blob/master/sample/prelude-modules.el 

I choose the following modules to enable with `prelude-lsp` and `prelude-ocaml` being the core OCaml related choices. The other bits are optional but useful for editing lisp or navigating code.

``` emacs-lisp
(require 'prelude-ivy) ;; A mighty modern alternative to ido
(require 'prelude-company)
(require 'prelude-emacs-lisp)
(require 'prelude-lisp) ;; Common setup for Lisp-like languages
(require 'prelude-lsp) ;; Base setup for the Language Server Protocol
(require 'prelude-ocaml)
```

Now for the customisation to get LSP working properly. 
There are 3 main pieces:

 * direnv - for automatically configuring shell environments
 * ocaml-lsp-server - the core lsp implementation for OCaml
 * lsp-mode - the Emacs mode that drives everything
 
direnv the necessary magic
----------

direnv is a small program to load/unload environment variables based on $PWD (current working directory). This program ensures that when you open an OCaml file the correct opam switch is chosen and the tools installed in that switch are made available to Emacs. Opam is the [OCaml package manager](https://opam.ocaml.org) and manages local sandboxes of packages called switches. Without direnv Emacs will not find the correct tools and you would need to mess with Emacs PATHS to get it right. I have done that and it is much simplier with direnv. 

So `brew install direnv` and create a `.envrc` file in an OCaml project with `eval $(opam env --set-switch)` inside. Compared to my previous post I have been using local opam switches which exist inside an OCaml project. They are created as `opam switch create . 4.14.0 --with-test --deps-only -y` and appear as an `_opam` directory in the project root. Next run `direnv allow` to tell direnv it is safe to use the `.envrc` file in this directory. The reason I have switched is I often need to test different OCaml versions so removing the `_opam` directory and recreating it is the simplier option.

OCaml LSP Server
----------

OCaml LSP server needs to be installed in the current switch so run `opam update && opam install ocaml-lsp-server -y`, this will make ocaml-lsp-server available to Emacs via direnv. 

There is an opportunity here to use Emacs Lisp to install `ocaml-lsp-server` if it was missing or to allow lsp-mode to download and install it itself. I would like to have this working in future. Next back into Lisp.

Emacs LSP mode
----------
Create a file init.el in `~/.emacs.d/tsmc/` substituting your Unix user name for `tsmc`. Thanks to emacs-prelude the configuration is very small.

``` emacs-lisp
;;; init.el --- @tsmc configuration entry point.

(prelude-require-packages '(use-package direnv))
;; Use direnv to select the correct opam switch and set the path
;; that Emacs will use to run commands like ocamllsp, merlin or dune build.

(use-package lsp-mode
  :hook
  (tuareg-mode . lsp))
;; Attach lsp hook to modes that require it, here we bind to tuareg-mode rather than
;; prelude-ocaml. For unknown reasons the latter does not bind properly and does not
;; start lsp-mode

(provide 'tsmc)
;;; init.el ends here
```

We require a few packages `use-package` and `direnv`, and then tell Emacs to start lsp-mode when `tuareg-mode` is started. Tuareg-mode is one of the OCaml modes available for Emacs, the other being `caml-mode` which I have not really used. Now quit and restart Emacs. Opening an ml file inside the project you started earlier and ocaml-lsp should startup. 

The types for expressions and modules will display on mouse hover or beside the definition. Hovering the mouse over a function or type will display the type plus the documentation comments for it. A successful `dune build` for the project is required to generate the data used by ocaml-lsp-server. At this point in time `prelude` relies on `merlin` an assistant for editing OCaml code, that is used by `ocaml-lsp-server` internally but also available as standalone tool. So I often have both installed, `opam install merlin` should be enough to get it installed too.

At this point I am mostly happy, the types and documentation displays as required. Navigating using `M-.` shows a preview of the type / function under point and return will take me to the definition. This is vastly improved in OCaml 4.14 (with the work on Shapes) which I have switched to for everything I can. Switching between ml and mli files is `C-c C-a` and more, simply visit the `M-x describe-mode` to show everything available.

The annoyances are more fundamental to how LSP wants to work. It uses what I am calling a push based interaction, where it generates the information for types and documentation in the background and pushes it into the Emacs buffer. You never need to ask what is the type, it will display for you. Sometimes I want to ask for what a type is inside an expression, with LSP you are encouraged to mouse hover over something rather than having a key binding for it. So far I haven't found the lisp function that drives the hover functionality but when I do I will bind it to a key. The second issue is also around mouse usage to drive LSP functionality like rename or annotate types. I would strongly prefer a key chord driven approach to that. Again I will set this up once I find the right lsp functions. For now I use `C-c C-t` from merlin to summon the types for things.

Overall the experience is solid. Types and docs appear as required. Navigation works. The speed has been good so far. LSP mode is less janky than it was 1 year ago.

Alternatives
---------- 

There is a fine alternative LSP mode, [Eglot](https://github.com/joaotavora/eglot) for Emacs. It takes a more minimal approach and uses a pull based interaction. Where you ask for the information based on key bindings vs the information being pushed at you via UI elements. For example, the type of a function is requested rather than shown by default.
 
The corresponding configuration I was using previously is:
 
``` emacs-lisp
(use-package eglot
  :config
  (define-key eglot-mode-map 
    (kbd "C-c C-t") #'eldoc-print-current-symbol-info)

  :hook
  ((tuareg-mode . eglot-ensure)))
```

Again using `use-package` to configure the mode, the hooks are triggering Eglot to be loaded when `tuareg-mode` is. Using the `eglot-ensure` function which starts an Eglot session for current buffer if there isn't one. No further configuration is needed in Emacs as Eglot knows the LSP server is called `ocamllsp` and will look for it on the Unix PATH.

Summary
----------

Getting started with OCaml using Emacs can be a struggle. Emacs is a fine editor but the documentation can be difficult to handle. Hopefully following through this setup will yield a working Emacs / LSP setup for OCaml.

In future I want to try binding more things to keys so I use the mouse less and streamline the installing of the ocaml lsp server. Then after that adding support for more interesting code interactions like extracting modules or hoisting let bindings would be nice to have. Happy hacking!




