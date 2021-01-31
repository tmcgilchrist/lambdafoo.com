---
layout: post
title: "ocaml quickcheck"
date: 2014-08-22 12:48
comments: true
categories: ocaml
published: false
---

Testing is a rather popular topic amongst all programming language comminities
and there is little doubt that you should be writing some form of tests for your
software. Even if you don't follow a strict Test Driven Development approach.
It's a great thing to see and certainly yields better softare that is more
reliable.

However unit tests you write yourself are only as good as the test data that you
supply for them. Often you'd write a few happy case scenarios and try to cover
all the edgecases but often things slip through. The unsurpising fact about
production bugs is that they slipped through your test and the type system
before being noticed. A better way of performing test is to state the propertes
that should hold true and get a bunch of generated data thrown at your function
to see where it breaks.

Enter [QuickCheck](github.com/avsm/quickcheck), a library for doing property
based testing. It frees you up from having to think of test data and lets you
focus on writing the properties your code should have. It's an immensley
powerful approach to testing and may even replace your use of traditional unit
tests.


Lets get started on an example.

First you'll need to install quickcheck via opam.

{% codeblock lang:bash %}

opam install quickcheck

{% endcodeblock %}


Starting with a likely looking implemenetation of quick sort.

{% codeblock lang:ocaml %}

let rec qsort gt = function
  | x::xs ->
      let ys, zs = List.partition_tf ~f:(gt x) xs in
      (qsort gt ys) @ (x :: (qsort gt zs))

{% endcodeblock %}

Here's the property we want our quick sort to hold

{% codeblock lang:ocaml %}

let prop_idempotent xs =
  (qsort (>) (qsort (>) xs)) = (qsort (>) xs)

{% endcodeblock %}

Basically it's saying if we qsort a list once it should be the same as qsorting
it twice.

Next we need to constuct a description of our function we want to test

{% codeblock lang:ocaml %}

let testable_list_to_bool =
  testable_fun (arbitrary_list arbitrary_int) (show_list show_int) testable_bool

{% endcodeblock %}

This says we have a testable_fun that takes an list of ints, returning a bool.
The middle show_list show_int is so quickCheck knows how to display the results
if it finds a contradicting test case.

Last we need to run the test.

{% codeblock lang:ocaml %}

let () =
  match quickCheck testable_list_to_bool prop_idempotent with
  | Success -> ()
  | Failure _ -> failwith "No failure expected"
  | Exhausted _ -> failwith "No exhaustion expected"

{% endcodeblock %}
