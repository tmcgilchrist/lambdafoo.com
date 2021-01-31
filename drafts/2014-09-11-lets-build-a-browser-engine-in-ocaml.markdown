---
layout: post
title: "lets-build-a-browser-engine-in-ocaml"
date: 2014-09-11 21:32
comments: true
categories:
 - ocaml
published: false
---

Browsing r/haskell the other day I saw the start of a series of posts by Leif
Grele where he was following along with Matt Brubeck's series of blog posts on
constructing a toy html rendering engine in Rust. Like Leif I have little
interest in Rust but I am generally curious about how a web browser works. So
I'm going to try following along in OCaml as best as I can.

## Build Environment

Matt is attempting to stay within the Rust standard library, but for OCaml the
standard library is less than fully featured so instead I'll use the Core
library from Jane Street. Where possible I'm going to stick with using v4.02 of
OCaml.

I'll call this endevour Icarus in light of my hubris at attempting to fly too
high. ** The Lament for Icarus by H. J. Draper **

## Starting in The DOM

TODO How to model the dom as an ADT?
