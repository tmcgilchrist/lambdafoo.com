---
layout: post
title: "Lenses in OCaml"
date: 2015-01-16 10:52
comments: true
categories:
 - ocaml
---

Lenses have been on my mind since encountering them last year in the context of
Haskell. Much of the literature on lenses has a very Haskell slant so show how
they can be used in OCaml.

The theory of lenses and their accompanying prisms and traversals, have been
better described by other people.
[This](https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial)
article at FPComplete was a particularly good one. I'm just going to cover how
to use [ocaml-lens](https://github.com/avsm/ocaml-lens/) as a minimal lens
implementation.

First since ocaml-lens isn't in opam, clone the repo locally and open up
`utop`. Then load the `lens.ml` file into `utop`.

{%codeblock lang:ocaml%}
utop # #use "lens.ml";;
..
{% endcodeblock %}

Starting with a few record types for a car, editor and book.

{%codeblock lang:ocaml%}
type car = {
    make : string;
    model: string;
    mileage: int;
  };;

type editor = {
    name: string;
    salary: int;
    car: car;
};;

type book = {
    name: string;
    author: string;
    editor: editor;
};;
{%endcodeblock%}

Creating a new book is as simple as.

{%codeblock lang:ocaml%}
let scifi_novel = {
   name =  "Metro 2033";
   author = "Dmitry Glukhovsky";
   editor =  {
     name = "Vitali Gubarev";
     salary =  1300;
     car =  {
       make = "Lada";
       model = "VAZ-2103";
       mileage = 310000
    }
  }
};;

{% endcodeblock %}

Given our `scifi_novel` we can access the editor's car mileage:

{%codeblock lang:ocaml%}
let mileage = scifi_novel.editor.car.mileage;;
{% endcodeblock %}

Setting the mileage is a bit trickier, we need to unpack each record:

{%codeblock lang:ocaml%}
let second_edition = { scifi_novel with editor =
                { scifi_novel.editor with car =
                    { scifi_novel.editor.car with mileage = 1000 } } };;
{% endcodeblock %}

That's not really an appealing prospect, can we do better?

Enter lenses, at the most simple level a lense is a pair of functions for
getting and setting a property.

{%codeblock lang:ocaml%}
(** Lens type definition *)
type ('a, 'b) t = {
  get : 'a -> 'b;
  (** Functional getter *)
  set : 'b -> 'a -> 'a
  (** Functional setter *)
}
{% endcodeblock %}

With this definition of a lens, modifying the mileage is now:

{%codeblock lang:ocaml%}
let a = compose mileage_lens (compose car_lens editor_lens) in
 _set 10 scifi_novel a;;
{% endcodeblock %}

In the background we need to define some lenses for the records above:

{%codeblock lang:ocaml%}
let car_lens = {
	    get = (fun x -> x.car);
	    set = (fun v x -> { x with car = v })
	  };;

let editor_lens = {
	    get = (fun x -> x.editor);
	    set = (fun v x -> { x with editor = v })
	};;

let mileage_lens = {
	    get = (fun x -> x.mileage);
	    set = (fun v x -> { x with mileage = v })

  };;
{% endcodeblock %}

Using these definitions the original lens version of modify the editor's car
mileage works.

The compose operator we used allows us to combine 2 lenses to go from the novel
into the editor and then into the car. And compose can be combined with itself
to build up arbitarily deep lenses into a structure.

{%codeblock lang:ocaml%}
let editor_car_lens = compose car_lens editor_lens;;
{% endcodeblock %}

This way of composing can seem backwards, you supply the inner lens first then
the outer lens. We can fix that by using the infix operators, open the `Infix`
module and define the same lens:

{%codeblock lang:ocaml%}
let editor_car_lens = editor_lens |-- car_lens;;
{% endcodeblock %}

This feels more intuative reading it left to right. Revisiting our original
`_set` mileage example we can now write it.

{%codeblock lang:ocaml%}
_set 10 scifi_novel (editor_lens |-- car_lens |-- mileage_lens);;
(* or even *)
((editor_lens |-- car_lens |-- mileage_lens) ^= 10) @@ scifi_novel;;
{% endcodeblock %}

The infix module comes with some other helpful operators like
`|.` for get and `^=` for set. All these operators avoid mutation so our
code remains pure and referentially transparent.

### Conclusion

There are a heap more things that lenses can do, and while this ocaml-lens
package is pretty basic, looking at the hundreds of functions exported by
[Control.Lens](http://hackage.haskell.org/package/lens)
in Haskell you can get a good idea of the possibilities. `Control.Lens` includes
all the basic lens functions plus things like:

  * `prisms` which are lenses but for sum types
  * `traversals` are lenses that focus on multiple targets simultaneously

### Resources

I made use of the following resources to write this and took some of the
examples and definitions from the following articles. All mistakes are my own
and probably accidental.


  * [A Little Lens Starter FPComplete](https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial#okay--what-are-prisms-)
  * [Lenses in F#](http://bugsquash.blogspot.com.au/2011/11/lenses-in-f.html)
  * [Control.Lens on hackage](http://hackage.haskell.org/package/lens)
