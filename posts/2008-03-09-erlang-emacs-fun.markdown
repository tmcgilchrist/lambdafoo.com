---
title: Erlang + Emacs = Fun
author: Tim McGilchrist
date: 2008-03-09 00:00
tags: erlang
description: Erlang + Emacs = Fun
---

As part of learning Erlang I needed a decent editor. I tried using
**vi** but it just didn't have the right level of integration. So
I'm now using **Emacs** and it seems to be a pretty good fit. I'm
by no means an advanced Emacs user and I'm sure there are plenty of cool things
you can do with Erlang but I you need to start somewhere.

So here is the configuration for getting an Erlang emacs mode, drop it into your
.emacs file.

``` common-lisp

(setq load-path (cons  "/opt/local/lib/erlang/lib/tools-2.6/emacs"
                       load-path))
(setq erlang-root-dir "/opt/local/lib/erlang")
(setq exec-path (cons "/opt/local/lib/erlang/bin" exec-path))
(require 'erlang-start)


```

I'm using the Erlang installed by MacPorts, which installs into **/opt/local**,
adjust the path for local conditions.

A few useful commands to get you started:

    C-c C-z (erlang-shell-display) - Start, or switch to, an inferior Erlang shell.
    C-c C-k (erlang-compile)       - Compiles the Erlang module in the current buffer.
    C-x `   (erlang-next-error)    - Move the point on to the next error.
    C-c C-c (comment-region)       - Comment or uncomment each line in the selected region.

where **C** is Control and the second element is a letter pressed at the same
time.

All the features of the Erlang mode are described by switching to Erlang mode (M
- erlang-mode) and describe the mode (M - describe-mode), where **M** is the
Meta key. Usually the **Alt** or **Option** key on a Mac.
