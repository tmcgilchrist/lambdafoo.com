---
layout: post
title: "From Applicative to Monad"
date: 2015-03-10 17:45
comments: true
categories:
 - ocaml
published: false
---
Following on from these 2 fantastic posts about
[implementing Functor in OCaml](http://blog.0branch.com/posts/2012-03-26-01-implementing-functor-ocaml.html)
and
[from Functor to Applicative](http://blog.0branch.com/posts/2012-03-26-02-from-functor.html)
which show how these concepts can be translated into OCaml. I'd like to add a
follow up post for the next logical step from Applicative to Monad. But first
lets review some of the terminology.

A functor is informally, a structure that provides a 'mapping' operation that
applies a value in a given context, while preserving that context. Commonly this
appears as providing an `fmap` operation. The structure will look like:

```ocaml
    module type Functor = sig
      type 'a t
      val fmap : ('a -> 'b) -> 'a t -> 'b t
    end
```

An applicative functor, also know as just applicative, extends upon `Functor` by
providing a function `pure` for lifting a value into the context and then `<*>`
given a function `val f: ('a -> 'b) t`from a to b within the context it applies
that to a vallue `a' t` which is already in the context and produce a `b' t`
within that context. The signature below captures these ideas.

```ocaml
    module type ApplicativeFunctor = sig
      include Functor

      val pure: 'a -> 'a t
      val (<*>): ('a -> 'b) t -> 'a t -> 'b t
      val (<$>): fmap
    end
```

There are a number of laws that apply to both these concepts, which are better
explained in the previous posts or in extended detail in the Haskell [Typeclassopedia]()

-- General idea of Monad
Following on

```ocaml
    module type Monad = sig
      include ApplicativeFunctor

      val bind : 'a t -> ('a -> 'b t) -> 'b t
      val return : 'a -> 'a t

      val (>>=): 'a t -> ('a -> 'b t) -> 'b t

    end
```

-- Example of Reader Monad

-- Example of State Monad

-- Combining Monads
