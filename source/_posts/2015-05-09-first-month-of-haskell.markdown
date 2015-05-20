---
layout: post
title: "First month of Haskell"
date: 2015-05-09 18:02
comments: true
categories:
  - haskell
  - ocaml
---

I've been excpetionally fortunate in the past month to accomplish a long held
goal of mine. As of the 13th of April I've been employed full time as a
functional programmer. In particular I've taken the deepest of dives into
Haskell. I thought it might be interesting, at least for me, to write up my
thoughts after completing a month of Haskell.

First the depth of the dive has been overwhelming and the learning curve more
equivalent to a vertical rock climb. But the entire time, no matter the
exhaustion and believe me there was a lot of that, has been extrodinary. When I
take a moment to reflect, I've had a smile on my face.

First thing, the degree to which types are ingrained in Haskell. That might seem
surprising in itself, Haskell is afterall a strongly typed langauge and it was
surprising to me too. I've used Erlang, with dialyzer, and OCaml a great deal
before starting, and both these languages have reasonable type systems. Ocaml is
even described as strongly typed. So what am I getting at?

Everything in Haskell feels typed to the nth degree. Every possible abstraction
is pulled out into a common place, either Applicatives, Monads, Bimaps or Monad
Transformers. Which is great that you can abstract like that. Using any Haskell
library will require you to know about some of these things.

Coming from a background where I'd done Lisp, Erlang and OCaml, and some Haskell
I thought I was totally prepared to start working in Haskell full time.

Learning Haskell the language is a good first step, but knowing the syntax and
being comfortable reading code is one thing. What really surprised me was that
knowing Haskell isn't sufficient, you need to learn the set of typical Haskell
libraries before you can really start making progress and feel at home in
Haskell. Of course I'd used things like Monads in OCaml and read

No equivalent to Monad, Monad transformers, lenses, applicative, traversable
library eco-system in OCaml. I naievely wonder why this hasn't been built
before and whether it's even a good idea. The parallels to Scala and scalaz
are all too apparent to me. Scala is a mixed OO/FP langauge in a similar way
to OCaml. It also doesn't enforce the same level of strictness with respect to
side effects that Haskell does. So in both langauges if you want you can
create a mess of side effectey code if you;re not careful. Also both langauges
allow mutation, again another side effect, without tracking this via the type
system.

I want to thank Mark Hibberd and Charles O'Farrell for being such great mentors
over the last month, may they never grow tired of my endless questions.
