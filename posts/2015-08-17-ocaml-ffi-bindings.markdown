---
layout: post
title: "ocaml ffi bindings"
date: 2015-08-17 08:14
comments: true
categories: ocaml
---

One thing that always comes up with your favourite language is how do you use
libraries written in another language. Typically this involves needing to talk
to a particular C library, either because it's faster than a native one or just
that it is already written.

For OCaml there is the ctypes library for binding to C libraries using pure
OCaml. Written by the people at the good people at OCaml Labs
[http://ocaml.io](http://ocaml.io)

The core of ctypes is a set of combinators for describing the structure of C
types -- numeric types, arrays, pointers, structs, unions and functions. You can
use these combinators to describe the types of the functions that you want to
call, then bind directly to those functions -- all without writing or generating
any C!

Lets go through a simple example binding to libyaml. Here's a declaration form
libyaml to get the version string.

{% codeblock lang:c %}

/**
 * Get the library version as a string.
 *
 * @returns The function returns the pointer to a static string of the form
 * @c "X.Y.Z", where @c X is the major version number, @c Y is a minor version
 * number, and @c Z is the patch version number.
 */

YAML_DECLARE(const char *)
yaml_get_version_string(void);

{% endcodeblock %}


To bind to this we need to declare a compatible signature for our OCaml code.

{% codeblock lang:ocaml %}

open Ctypes
open Foreign

let get_version_string =
  foreign "yaml_get_version_string"
    (void @-> returning string)

{% endcodeblock %}

We're pulling in Ctypes and Foreign. Then the let binding is using foreign with
the name of the c method we want to call plus a type signature for that method.

Next we need some calling code to print out the version string.

{% codeblock lang:ocaml %}
open Core.Std

let () =
  let version_string = get_version_string() in
  printf "Version: %s\n" version_string

{% endcodeblock %}

Assuming you've got opam installed you can get the dependencies `opam install
core ctypes` and compile the whole thing.

{% codeblock lang:bash %}

> corebuild -pkg ctypes.foreign -lflags -cclib,-lyaml version_string.native
...
./version_string.native
Version: 0.1.6

{% endcodeblock %}

We've got bindings to a native C library without writing any C.

More complicated example involving passing an allocated string back from C, lets
look at the `proc_pidpath` call from OSX. This particular library call takes a
process id (PID) and returns back

{% codeblock lang:c %}
int
proc_pidpath(int pid, void * buffer, uint32_t  buffersize)
{% endcodeblock %}

To bind to this call we again define a compatible signature.

{% codeblock lang:ocaml %}
let pidpath =
    foreign ~check_errno:true "proc_pidpath"
            (int @-> ptr char @-> int @-> returning int)
{% endcodeblock %}

The arguments simply mirror those for the C library call, along with a new
argument `check_errno` which indicates the c library sets errno if it encounters
a problem.

http://stackoverflow.com/questions/22651910/returning-a-string-from-a-c-library-to-ocaml-using-ctypes-and-foreign

Ctypes provides native bindings for most things you'll need. There's all sorts
of pointers and types matching pretty much every native C type you'll need
[here](https://github.com/ocamllabs/ocaml-ctypes/blob/master/src/ctypes/ctypes.mli).
