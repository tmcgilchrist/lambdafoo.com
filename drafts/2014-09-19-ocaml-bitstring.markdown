---
layout: post
title: "ocaml-bitstring"
date: 2014-09-19 14:41
comments: true
categories:
  - ocaml
published: false
---

One of the coolest features in Erlang is it's built in support for pattern
matching against binaries. The bit syntax is a pattern matching syntax used for
packing and unpacking bit fields from raw binaries. Think something like
unpacking a binary format like the mysql wire protocol or MP3 headers. Not
really something you use all the time but when you need it very veyr useful.

Introducing the bitstring library for Ocaml, which adds Erlang-style bitstrings
and matching over bitstrings as a syntax extension for OCaml.

Lets work through an example of extracting ID3 tags from mp3 files.
