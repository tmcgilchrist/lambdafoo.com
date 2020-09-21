
Scala's Modular Roots
---------------

I'd like to start with the disclaiminer I'm no real expert in Scala, and the
total sum of my scala output is possibly below everyones in this room.

Having said that I do have a deep interest in programming languages and how the
various parts of a language fit together. How they influence other parts. And
how those parts compare to similar concepts in other langauges.

To me the most interesting part of Scala is how it has both OO and FP
influences. This leads to a somewhat unique place for it. The only other popular
(sort of) language that has a similar mix of OO and FP is OCaml. Which I'm also
interested in. I'm that sort of person, contrarian maybe.

So when after a particular Scala Syd or perhaps it was FP-SYD, Tony Sloane
mentioned that there was a correspondence between Scala and OCaml I became
interested in chasing down that information.

Martin Odersky did a series of talks at GOTO Chicago and flatMap Oslo where he
spoke about Scala the Simple Parts.

So here I'm presenting an interesting look at the inspiration Odersky drew from
the ML families' module system.

First it's worthwhile introducing the key concepts from ML modules.
The ML family of languages are static, strongly typed langauges with strict
evaluation.

The key parts of OCaml's module system are:

* Structures
* Signatures
* Functors

OCaml Modules
---------------
* Structures group together types and functions with implementations
* Signatures interface for a structure
* Functors are functions from `struct` to `struct`

??? Code example perhaps, something simple

Scala as ML
---------------

Object ≅ Structure
Class ≅ Functor
Trait ≅ Signature
Abstract Type ≅ Abstract Type
Refinement ≅ Sharing Constraint

The slide in particular that discusses this correspondence is above.
I'm assuming the values on the left hand side are mostly familar to the audience
here, while those on the right may be less so. Users of `scalaz` can probably
make sense of a few of them however.

Don't stress about that too much, I'll introduce the right hand side and if I do
this correctly you'll have a reasonable idea what they are.

Signatures as Traits
---------------
Going back to ML land, lets introduce an example of a Set. With a set we're
going to need a way of comparing elements to ensure our set like properties
hold.

``` ocaml
module type ORDERING = sig
  type t
  val compare : t -> t -> int
end
```

ORDERING is an OCaml signature, specifying the type `t` and a function `compare`
that takes 2 `t`'s returning an int. This int ranges from -1 to 1, depending if
the first value is less than, equal or greater than the second argument.

A similar Ordering concept is contained in the Scala standard libary

```scala
trait Ordering[T] {
  def compare(x: T, y: T): Int
}
```

This is the first point of Odersky's equivalence: signatures and traits.

Leaving aside that ML signatures are pure, while Scala trais can contain an
implementation, the `Ordering` trait is a paramterised type.

We can re-write the Scala version to be closer to OCaml by making the positional
type parameter an abstract type member.

```scala
trait Ordering {
  type T

  def compare(x: T, y: T): Int
}
```

Now baring minor syntax differences like the curly brackets, they're looking
rather similar.

????  discussion around type parameters versus abstract type members

```ocaml
module type SET = sig
  type t
  type set
  val empty : set
  val insert : t -> set -> set
  val member : t -> set -> bool
end
```

```scala
trait SetSig {
  type Elem
  type Set

  def empty: Set
  def insert(e: Elem, s: Set): Set
  def member(e: Elem, s: Set): Boolean
}
```

Structures as Objects
---------------
The next part of the equivalence is between OCaml structures and Scala's objects.

A structure in OCaml is defined as:

```ocaml
module IntOrdering = struct
  type t = int
  let compare(x,y) = x - y
end
```
This introduces a named struct `IntOrdering` with 2 elements, one being the type
t and the second being a function called compare

```scala
object IntOrdering extends Ordering {
  type T = Int

  def compare(x: Int, y: Int): Int = x - y
}
```

Again baring slight syntax differences the 2 code examples look very similar.

ML Functors as Classes
---------------
Now we come to the next part of Odersky's equivalence argument, that Scala
Classes are equivalent to OCaml Functors. Assuming most/all people here are
familar with the Scala side of the argument. So what exactly is an OCaml
functor?

An OCaml functor is a function within the module language of OCaml, that
parameterises one structure by another structure. SO they're a functor in the
Category Theory sense, of being a mapping from one thing to another, just at a
module level.

Perhaps some code will make it clearer:

```ocaml
module UnbalancedSet (Element : ORDERED) : (SET with type t = Element.t) = struct
  type t = Element.t
  type tree = E | T of tree * t * tree
  type set = tree

  let empty = E

  let rec member x = function
    | E -> false
    | T(a, y, b) when Element.compare x y < 0 -> member x a
    | T(a, y, b) when Element.compare x y > 0 -> member x b
    | _ -> true

  let rec insert x = function
    | E -> T(E, x, E)
    | T(a, y, b) when Element.compare x y < 0 -> T(insert x a, y, b)
    | T(a, y, b) when Element.compare x y > 0 -> T(a, y, insert x b)
    | T(a, y, b) as s -> s
end
```

This example is taken from Purely Functional Data Strucutres by Okasaki. It's a
set implemented as a binary tree, and is formulated as a functor so that it is
parametric in it's elements and their ordering.

Reading this from the top, the functor takes 2 structures, the first is
something ORDERED which is bound to the name `Element` within the functor. The
second is SET where we bind the type inside the set `t` to the type inside
ORDERED `Element.t`


Extended Scala
---------------

First we will introduce a trait equivalent to the OCaml signature.

```scala
trait SetSig {
  type Elem
  type Set

  def empty: Set
  def insert(e: Elem, s: Set): Set
  def member(e: Elem, s: Set): Boolean
}
```

Nothing partcicularly surprising here, it's almost a 1 for 1 translation.
Given the previous definitions of `Ordering` and `IntOrdering`.

```scala

abstract class UnbalancedSet(o: Ordering) extends SetSig {
  val Element: Ordering = o
  type Elem = Element.T
```

Introducing an abstract class that extends `SetSig` and we need to do some extra
work to bind the type defined in the Ordering `o` to the type used inside the
`UnbalancedSet`. Ideally I'd like to just go

```scala
class UnbalancedSet(val Element: Ordering) extends SetSig {
  type Elem = Element.T
  ...
}
```
However type inferencing fails to unify the 2 types. So we need to provide some
further hints to the compiler. Expanding the val Element into the class body and
linking the Ordering to the value for Element yields a working solution.

Then to call our Scala functor we simply need to provide an implementation for
`Ordering`

```scala
object S extends UnbalancedSet {
  val Element: Ordering = IntOrdering
}
```

```scala
scala> var set = S.insert(1, S.empty)
set: S.Set = Branch(Leaf,1,Leaf)

scala> set = S.insert(2, set)
set: S.Set = Branch(Leaf,1,Branch(Leaf,2,Leaf))

scala> S.member(2, set)
res5: Boolean = true

scala> S.member(4, set)
res6: Boolean = false
```

Discovery of Path Dependent Types
---------------

In all this I was showing Charles my code translating OCaml modules into some
rather non-standard Scala code. And I was asking him about the example where I
wrote essentially an OCaml functor in Scala, I was having trouble getting the
code to type check how I wanted it to look.
Charles being a sharp guy said something along the lines of, "you probably don't
know it but you're using Path Depedent Types". Being the neophite in Scala I
assured him I didn't know WTF he was talking about. A short detour followed and
I started thinking about what the real differences are between the 2.

What are Path Dependent Types in Scala?
---------------


Where OCaml modules differ?
---------------
In OCaml signatures are structural types, while Scala traits are nominal
(*Reference*). What this means is that an OCaml signature can be matched by any
appropriate module after the fact. Whereas for Scala objects you need to declare
the relation at definition time. (*Reference*)

Structural types match on the requirements imposed by the signature, without
requiring any declarations. While, nominal matching is based on the explicit
declaration of relationships amoung signatures. Structural matching affords
greater flexibility, there's no requirement to declare the relationship between
2 signatures. Unintended matching can occur however.

While nominal matching is more restrictive requiring any matches to be
explicitly stated. Hence the need to declare all relationships (*Reference*)

OCaml signatures can be composed structurally using `include` and `where`.

In all other cases, generative functors have the correct semantics. Pretty much
the whole use-case of putting abstract types in a functor is that you can then
reason intensionally about them (i.e. distinguish them based on their access
path). This is super useful, for instance, if you have a notion of "index" or
something and you want to prevent yourself from using indexes from one table in
another one, or something along those lines.

This is what the Scala people mystifyingly call "path dependent types". It's
just generative abstraction.

Conclusion
---------------
 * Interesting to think about the equivalence between features in different
   langauges. It's what libraries like `scalaz` and `shapeless` have done in
   bringing Haskell Type Classes across into a language that wasn't designed for
   it.
 * Sometimes features can have un-intended repercussions, I'm not entirely sure
   Odersky had Path Dependent Types in mind when designing Scala
 * Exploring features in other languages, inevitably leads to a better
   understanding of your existing language. Again, in showing Charles the OCaml
   code in these slides he got a better understainding for the OCaml module
   system.

References
---------------
* Scala: The Simple Parts - Martin Odersky (https://www.youtube.com/watch?v=P8jrvyxHodU&feature=youtube_gdata_player)
*
