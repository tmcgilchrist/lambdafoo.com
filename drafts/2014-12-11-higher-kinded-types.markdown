---
layout: post
title: "Higher kinded types in Ocaml"
date: 2014-12-11 08:03
comments: true
categories:
 - OCaml
published: false
---

Establish the problem domain first.

### What
Higher kinded types abstract over type constructors. Natively languages like
Haskell provide this unctionality witing the core language. With OCaml however
we need to work a little harder to build this feature. Looking at the language
we seem that functions themselves cannot be higher kinded. The only place where
we might have a chance is with records; where we can define a record with
polymorphic fields.

```
    type sqs_info =
      { queue_name : string;
        queue_url  : string};;
```

### Why

Polymorphism abstracts types eg

Functions abstract values

val double : int -> int = <fun>
let double x = x * 2;;

### Compare to Haskell

Defining when in haskell gives:

    Prelude> let when b m = if b then m else return ()
    Prelude> :t when
    when :: Monad m => Bool -> m () -> m ()

Using when inside haskell is simple and doesn't require any special code.

    Prelude> let unless b m = when (not b) m
    Prelude> :t unless
    unless :: Monad m => Bool -> m () -> m ()

How is it clear that this definition is higher-kinded???
### Using Functors in OCaml

    module type Monad = sig
      type 'a t
      val bind: 'a t -> ('a -> 'b t) -> 'b t
      val return: 'a -> 'a t
    end;;

    module When (M : Monad) = struct
      let f b m = if b then m else M.return ()
    end;;

    (* Gives the following type
    module When :
      functor (M : Monad) -> sig val f : bool -> unit M.t -> unit M.t
    end
    *)

    module Unless(M : Monad) = struct
      module W = When(M)
      let f b m = W.f (not b) m
    end;;

Using Unless in Ocaml is less straight forward, first we need to instantiate
Unless with a Monad then pass that around where we wish to use Unless. If we
want to use a different Monad we need to instantiate it again.

    module type State = sig
      type t
      val empty: t
    end;;

    module type StateM =
      functor(State : State) ->
        sig
          include Monad
          val access : 'a t -> 'a
          val put : State.t -> unit t
          val get : State.t t
      end;;

TODO provide an implementation of StateM based on the haskell code


### Using abstract type app

Source Code here:
https://github.com/ocamllabs/higher/
