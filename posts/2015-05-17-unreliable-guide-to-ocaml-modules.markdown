---
title: Unreliable guide to OCaml modules
author: Tim McGilchrist
date: 2015-05-15 19:13
tags: ocaml
description: Unreliable guide to OCaml modules
---

Being on the curious side of things I have been interested lately in the dualities
between programming languages. Like how one feature say Type Classes in
Haskell compares to what is available in Scala or OCaml. This has lead to me
reading a substantial amount of academic papers about the subject.

So with that in mind I would like to give a brief introduction to OCaml style
modules. Perhaps in another post going into how can you encode something like
rank n types from Haskell in OCaml which natively doesn't support them.

Preface, the use of the word module can be confusing, and it sometimes seems
that module is used to refer to structures interchangeably. I've tried to avoid
that but it's helpful to keep in mind for further reading. Look at what's on the
right hand side of the equals in the code. Let start.

Terminology
---------------------

OCaml is a member of the ML family of languages, sharing common features like
modules,
[Hindley-Milner type sytem](http://en.wikipedia.org/wiki/Hindley-Milner_type_system)
and strict evaluation. OCaml as a language can be though of 2 distinct part; one
a core language that's values and types and a second module language that
revolves around modules and signatures. While OCaml does provide some support
for bridging these parts in the form of First Class Modules, I won't cover them here.

The key parts of the module system in OCaml are:

* Structures
* Signatures
* Functors

Structures
---------------------
Structures provide a way for grouping together related declarations like data
types and functions the operate on them; they also provide the values in the
module langauge. Below is a module for integer Sets:

``` ocaml
    module IntSet = struct
      type t = int
      type set = t list
      let empty = []
      let member i s = List.exists (fun x -> x = i) s
      let insert i s = if member i s then s else (i::s)
    end
```

This code defines a new structure using the `struct` keyword and binds it to a name
using module. It's useful to note that OCaml types are written in lowercase (`t`,
`list` and `set`) and type variables are written with a single quote `'a`. Also
type constructors are written differently to Haskell, in Haskell you'd have
`List a` while in OCaml the order is reveresed `t list`.

Basically a struct is an opening `struct` followed by a bunch of `type` and
`let` bindings, and closed with an `end`.

At the call site exposed declarations are referred to by dot notation:

``` ocaml
    IntSet.t
    IntSet.empty
```

If no module name is defined within a file, say you have a file called `set.ml`
with:

``` ocaml
      type t = int
      type set = t list
      let empty = []
      let member i s = List.exists (fun x -> x = i) s
      let insert i s = if member i s then s else (i::s)
```

It will implicitly be given a structure name derived from the file name `Set` but
as you may have worked out module names are not bound to file names. Further
structures can be nested within other structures, leading to more freedom than
just having 1 file becoming 1 module.

``` ocaml
    module IntSet = struct
      module Compare = struct
         type t = int
         let eql x y = x = y
      end
    end;;
```
The values within the nested module are referred to like so:

``` ocaml
    IntSet.Compare.eql 1 1;;
```

While it is great to have functions namespaced like so, it would become tedious
if you needed to use the longer name to refer to a nested module. OCaml provides
a couple of solutions, first local opens.

Rather than having an `open` statement at the top of the file and bringing
every thing into scope for that file we can do a local open and restrict the
scope to between the two brackets.

``` ocaml
     IntSet.Compare.(eql 1 1);;
```

The other option available is aliasing the module name to something shorter

``` ocaml
     module X = IntSet.Compare;;
     X.eql 1 1;;
```

I mentioned `open` before without saying what it does. Simply open brings the
contents of a module within another module, so they can be referred to without
the module name prefix.

Signatures
---------------------

Signatures are the interfaces for structures, a signature defines what parts of
a structure is visable from the outside. A signature can be used to hide
components of a structure or export some definitions with more general types.

A signature is introduced with the `sig` keyword

``` ocaml
    module type Set =
      sig
        type elt
        type t

        val empty : t
        val member : elt -> t -> bool
        val insert : elt-> t -> t
      end
```

As you can see looking at our definition of Set, it lists a type and function
signatures without specifying a concrete implementation. It's also bound to a
name `Set` using `module type`.

As I metnioned before signatures are typically used to hide or change the
interface a module exposes. By default all types and functions are exported from
a module. Useful for doing things like hiding implementation details or only
construct the data type via the invariant-preserving operations that the module
provides.

Typically in OCaml you'll define your `struct` in one file `set.ml` and then
create a second file `set.mli` which contains the signature for the module set.
Only occasionally will you see the signature and structure defined together.

Functors
---------------------
Now to the functors, they're not exactly like Haskell's though they do perform a
kind of mapping.

Functors are for lifting functions into the module language, or another way
they are `functions` from structures to structures. Which brings the abstract
idea of functors from category theory back to 2 concrete examples, where
Haskell functors are `functions` from types to types, OCaml's functors are
`functions` from structures to structures.

Following out set example we can make set operations abstract across both the
type inside the set and the equality comparison.

``` ocaml
    module type ORDERING =
      sig
        type t
        val compare : t -> t -> int
      end;;

    module type Set =
      sig
        type elt
        type t
        val empty : t
        val member : elt -> t -> bool
        val insert : elt-> t -> t
      end;;

    module MkSet (Ord : ORDERING) : (Set with type elt := Ord.t) =
      struct
        type elt = Ord.t
        type t = Empty | Node of t * elt * t

        let empty = Empty

        let rec insert x = function
          | Empty -> Node(Empty, x, Empty)
          | Node(a, y, b) when Ord.compare x y < 0 -> Node(insert x a, y, b)
          | Node(a, y, b) when Ord.compare x y > 0 -> Node(a, y, insert x b)
          | Node(a, y, b) as s -> s

        let rec member x = function
          | Empty -> false
          | Node(l, v, r) ->
              let c = Ord.compare x v in
              c = 0 || member x (if c < 0 then l else r)
    end;;

    module IntOrdering = struct
        type t = int
        let compare x y = Pervasives.compare x y
      end;;

    module IntSet' = MkSet(IntOrdering);;
```

Here we define `ORDERING` and `Set` as signatures, similar to our previous
definitons. Then a functor is defined `MkSet` that takes the `ORDERING`
signature and defines the types and functions for set based off that interface.
So the definition of `MkSet` is completely abstracted away from the type used in
the set and the functions used on those types. As long as it implements
`ORDERING`.

The last part defines a particular ordering for `int` using, binding t to `int`
and compare to `Int.compare`.

Using Modules
---------------------
After covering what is in the OCaml module system, what exactly do we use it
for. At the very basic level we collect together types and functions, which is
pretty much what all modules do. Outside of that we can:

1. Hide implementation details, like the types exported by the module. If we
   wanted to hide how our Set was implemented we could redefine the functor as:

``` ocaml
module type SETFUNCTOR =
    functor (O: ORDERING) ->
      sig
        type t = O.t      (* concrete *)
        type set          (* abstract *)
        val empty : set
        val add : t -> set -> set
        val member : t -> set -> bool
      end;;
```

Here we expose the elements within the set via `type t = O.t` so they're a
concrete type, while `set` isn't given a definition so the consumers of this
module can't look into that type without using the functions provided in the Set
module. This hiding using abstract types lets us swap out different
implementations for testing purposes or if requirements change.

2. Namespace functions and type, all types and functions live within some
   module.

3. Extending existing modules in a type safe way. You may want to extend a
   module from a library with extra derived functions. For example the `Core`
   library from Jane Street extends the built in OCaml library with a number of
   new and different functions. eg Say Lists didn't provide a `transpose`
   function.

4. Instantiating modules with State, OCaml allows modules to include mutable
   state (while we may not particularly like mutable things) sometimes it's
   necessary and you may want multiple instances of a particular module with
   their own state. Functors make doing this more succinct.

5. Collecting definitions and exporting as a single module, e.g. Core.Std inside
  Jane Street Core library.


Further Reading
---------------------

The best reference is really [Real World OCaml](http://realworldocaml.org). If
you've got some Haskell experience and don't mind reading a paper then "ML
Modules and Haskell Type Classes: A Constructive Comparison" by Stefan Wehr and
Manuel Chakravarty gives a thorough coverage of how ML modules stack up to Type
Classes.
