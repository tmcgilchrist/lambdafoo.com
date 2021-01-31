---
layout: post
title: "Type Driven Development in OCaml"
date: 2015-02-11 19:13
comments: true
categories:
  - ocaml
---

``` haskell
module Holes where

import Control.Monad

compose :: (b -> c) -> (a -> b) -> a -> c
compose = undefined

apply :: Monad m => m (a -> b) -> m a -> m b
apply = undefined

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' = undefined
```


``` ocaml
    String.concat "," (List.map _ [1;2;3])

    let value_exn = function Some x -> x;;
    let x = ref None;;

    let _ = fun () -> String.concat "," (List.map (value_exn !x) [1;2;3]);;
    x;;

    module Hole : sig
     val compose : ('a -> 'c) -> ('a -> 'b) -> 'a -> 'c
    end = struct
      let compose = value_exn !x
    end

(*
Error: Signature mismatch:                                                                      Modules do not match:                                                                      sig val compose : '_a end
       is not included in
         sig val compose : ('a -> 'c) -> ('a -> 'b) -> 'a -> 'c end
       Values do not match:
         val compose : '_a
       is not included in
         val compose : ('a -> 'c) -> ('a -> 'b) -> 'a -> 'c
*)


```

Gives us `(int -> bytes) option ref = {contents = None}` so we know the type is
`int -> bytes`
