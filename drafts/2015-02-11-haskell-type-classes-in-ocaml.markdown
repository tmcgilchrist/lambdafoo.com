---
layout: post
title: "Haskell Type Classes in OCaml"
date: 2015-02-11 08:01
comments: true
categories:
  - ocaml
  - haskell
---
- show how to convert haskell examples in papers to the equivalent in OCaml.
- simple example of an ocaml functor


Haskell Type Classes in OCaml
==============================

Reading academic papers on the subject of functional programming,
particular around types, can be a frustrating experience for an OCaml
programmer. Most of the examples are written in Haskell, which means they use
Haskell typeclasses. OCaml doesn't have these, instead it has high-order modules
(aka functors) which are modules that can take other modules as parameters.

Functors
--------

Give a simple example of a functor here

Now that we know what a high-order module is lets start on converting a Haskell
example to. Given the standard Haskell class `Show` which is used to convert
values to Strings.

```
Prelude> :type show
show :: Prelude.Show a => a -> String
```

Any new type class can derive show to get the ability to convert to a string.

```
data BookInfo = Book Int String [String]
                deriving (Show)

```
