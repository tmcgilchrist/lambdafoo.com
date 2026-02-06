---
layout: post
title: "Building OCaml from assembly"
date: 2022-08-24 12:00
comments: false
categories:
  - ocaml
  - compilers
---

At work I've been focusing on improving the debugging experience with OCaml.
As part of that I've discovered how some of the pieces fit together, that might
be obvious in retrospect, but are interesting to at least me so I'm going to
post details about them here.

The first nugget is you can hand compile an OCaml program into a final executable.
What do I mean? You can ask the OCaml compiler to output all the assembly generated
that goes into a library or executable. Then take that an call the assembler yourself
to build it. First lets review how the compiler works.

## Compilation Pipeline

Here is a _grossly_ simplified overview of the OCaml compiler. We feed in OCaml source code
in the form of ml/mli files, which flow through each stage and eventually end up
being emitted as either object files or textual assembly files. The first step from
OCaml Source to Parse Tree uses [menhir](https://gallium.inria.fr/~fpottier/menhir/) to parse
and generate an untyped [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree) representing
the code in the source file. This is then type
checked into a typed tree, this is where the type theory happens. After that, there are some stages
where the typed tree is transformed into representations more suitable for generating assembly.
The final stage traverses the CMM/Linear AST generating assembly code for a specific
family of CPUs (like x86_64 or ARM64).

```
                                      
 ┌──────────────┐   ┌──────────────┐  
 │ OCaml Source │   │  Parse Tree  │  
 │              ┼───►              │  
 └──────────────┘   └──────┬───────┘  
                           │          
 ┌──────────────┐   ┌──────▼───────┐  
 │    Lambda    │   │  Typed Tree  │  
 │              ◄───┼              │  
 └──────┬───────┘   └──────────────┘  
        │                             
 ┌──────▼───────┐   ┌──────────────┐  
 │  CMM/Linear  │   │    Emit      │  
 │              ┼───►   Assembly   │  
 └──────────────┘   └──────────────┘  
                                      
```

Finally, this assembly is compiled by the system C compiler to produce object files or
executables to be run. So we could treat the OCaml compiler as a _fancy_ way to
just generate assembly files, which we can then mess with to do things like add [DWARF
information](https://dwarfstd.org) or optimise assembly routines, or just for pure fun.

## OCaml source

Starting with an OCaml program taken from [Retrofitting Effect Handlers onto OCaml](https://doi.org/10.1145/3453483.3454039). This program doesn't compute anything interesting but it does show how OCaml's FFI to C works and how to pass control between the two. So it is interesting for what it does.

``` ocaml
$ cat meander.ml
external ocaml_to_c
         : unit -> int = "ocaml_to_c"
exception E1
exception E2
let c_to_ocaml () = raise E1
let _ = Callback.register
          "c_to_ocaml" c_to_ocaml
let omain () =
  try (* h1 *)
    try (* h2 *) ocaml_to_c ()
    with E2 -> 0
  with E1 -> 42
let _ = assert (omain () = 42)

$ cat meander_c.c
#include <caml/mlvalues.h>
#include <caml/callback.h>

value ocaml_to_c (value unit) {
    caml_callback(*caml_named_value
                  ("c_to_ocaml"), Val_unit);
    return Val_int(0);
}
```

Reading from the bottom of the file, `meander.ml` asserts that the function `omain`
returns the value `42`. It gets that value by calling `ocaml_to_c` which is actually an
external C function defined in `meander_c.c`, imported into OCaml using
`external` in the first line of `meander.ml`. The C function calls back into
OCaml using `caml_callback` which executes the `c_to_ocaml` function. An exception is
raised, unwinding everything back to `omain` with it's try/with blocks.

To compile this program we use the OCaml 5.2 compiler.

``` shell
$ ocamlopt --version
5.2.0
$ ocamlopt meander_c.c meander.ml -o meander.exe
$ ./meander.exe
$ echo $?
0
```
Running the program under macOS gives a successful exit code, so it must have got
`42` and the assertion passed. Try changing the value 42 to something else to check.

Next we will pull apart what the compiler is doing to generate the final
executable. Run `ocamlopt` with these flags:

``` shell
 $ ocamlopt meander_c.c meander.ml -o meander.exe -S -g -dstartup -verbose

+ cc  -O2 -fno-strict-aliasing -fwrapv -pthread -pthread  -D_FILE_OFFSET_BITS=64 -c -g -I'/Users/tsmc/code/ocaml/owee/_opam/lib/ocaml' 'meander_c.c'
+ cc -c -Wno-trigraphs  -o 'meander.o' 'meander.s'
+ cc -c -Wno-trigraphs  -o '/var/folders/z_/7yzlrkjn6pd441zs1qhzpjv00000gn/T/camlstartup9b503b.o' 'meander.exe.startup.s'
+ cc -O2 -fno-strict-aliasing -fwrapv -pthread  -pthread   -o 'meander.exe'  '-L/Users/tsmc/code/ocaml/owee/_opam/lib/ocaml'  '/var/folders/z_/7yzlrkjn6pd441zs1qhzpjv00000gn/T/camlstartup9b503b.o' '/Users/tsmc/code/ocaml/owee/_opam/lib/ocaml/std_exit.o' 'meander.o' '/Users/tsmc/code/ocaml/owee/_opam/lib/ocaml/stdlib.a' 'meander_c.o' '/Users/tsmc/code/ocaml/owee/_opam/lib/ocaml/libasmrun.a'     -lpthread
```

Focusing on the `ocamlopt` command, the flag `-S` asks the compiler to generate the assembly
files for the OCaml source, `-g` asks for debug information to be included, `-dstartup`
generates the startup file that bridges between the C startup and OCaml (more on that later)
and `-verbose` tells `ocamlopt` to print out what commands it's running.

So, what has been printed out? The first line is compiling the `meander_c.c` file into
an object file, the `meander_c.o` file in the current directory. Then we have a `meander.s`
file being compiled (assembled) into another object file. This is the output of compiling
the `meander.ml` OCaml source into assembly. The `--verbose` option doesn't show how that
file gets created. The third line is compiling the startup file from `meander.exe.startup.s`
into another object file. The final step is calling the linker via `cc` to generate the final
`meander.exe` file. You can see all the object files from previous steps plus the OCaml stdlib
`_opam/lib/ocaml/stdlib.a` and `_opam/lib/ocaml/std_exit.o` from the local opam switch
plus the OCaml libraries being added to the search path as
`-L/Users/tsmc/code/ocaml/owee/_opam/lib/ocaml`. It is not that dissimilar to building a
C program.

What about those assembly files? The `meander.s` is our ARM64 assembly file for `meander.ml`
open it up and search for `entry`. If you're on Linux or another architecture like x86_64
the assembly will be different but the names will be the same. This is the entry point
called when executing the program, the OCaml runtime jumps to the symbol `_camlMeander.entry`.

``` assembly
	.globl	_camlMeander.entry
L114:
	mov	x16, #34
	stp	x16, x30, [sp, #-16]!
	bl	_caml_call_realloc_stack
	ldp	x16, x30, [sp], #16
_camlMeander.entry:
	.cfi_startproc
	ldr	x16, [x28, #40]
	add	x16, x16, #328
	cmp	sp, x16
	bcc	L114
	sub	sp, sp, #16
	.cfi_adjust_cfa_offset	16
	.cfi_offset 30, -8
	str	x30, [sp, #8]
```

Search for other symbols like `omain` and `c_to_ocaml`
``` assembly
	.globl	_camlMeander.omain_278
_camlMeander.omain_278:
	.loc	1	8
	.cfi_startproc
	sub	sp, sp, #16
	.cfi_adjust_cfa_offset	16
	.cfi_offset 30, -8
	str	x30, [sp, #8]
....
_camlMeander.c_to_ocaml_273:
	.file	1	"meander.ml"
	.loc	1	5
	.cfi_startproc
	sub	sp, sp, #16
	.cfi_adjust_cfa_offset	16
	.cfi_offset 30, -8
	str	x30, [sp, #8]
```

All the code is there, we just need to assemble it. On my machine (macOS ARM64) running this
command will give me an executable `meander.exe` without even using `ocamlopt`.

``` shell
$ gcc -O2 -fno-strict-aliasing -fwrapv -pthread -D_FILE_OFFSET_BITS=64 \
      -c -g -I'/Users/tsmc/code/ocaml/owee/_opam/lib/ocaml' 'meander_c.c'
$ gcc -c -Wno-trigraphs -o 'meander.o' 'meander.s'
$ gcc -c -Wno-trigraphs -o meanderCamlStartup.o meander.exe.startup.s
$ gcc -o 'meander.exe' '-L/Users/tsmc/code/ocaml/owee/_opam/lib/ocaml' 'meanderCamlStartup.o' \
       '/Users/tsmc/code/ocaml/owee/_opam/lib/ocaml/std_exit.o' 'meander.o' \
       '/Users/tsmc/code/ocaml/owee/_opam/lib/ocaml/stdlib.a' 'meander_c.o' \
       '/Users/tsmc/code/ocaml/owee/_opam/lib/ocaml/libasmrun.a' -lpthread
```

Try it out, you'll need to change `/Users/tsmc/code/ocaml/owee/_opam` to your local directory with
a local opam switch for OCaml 5.2.

## Startup file

What about that startup file? `meander.exe.startup.s` What is that for?
Open the file and search for `_caml_program`, this is the entry point called by the
startup code written in C.

``` shell
_caml_program:
	.cfi_startproc
	ldr	x16, [x28, #40]
	add	x16, x16, #328
	cmp	sp, x16
	bcc	L136
	sub	sp, sp, #16
	.cfi_adjust_cfa_offset	16
	.cfi_offset 30, -8
	str	x30, [sp, #8]
L135:
	bl	_camlCamlinternalFormatBasics$entry
L137:
	adrp	x0, _caml_globals_inited@GOTPAGE
	ldr	x0, [x0, _caml_globals_inited@GOTPAGEOFF]
	ldr	x2, [x0, #0]
	add	x3, x2, #1
	dmb	ishld
	str	x3, [x0, #0]
	bl	_camlStdlib$entry
```

The code is responsible for calling the `entry` initialisation function for all
imported modules. In `meander.ml` we only include a couple of functions from the
standard library so we have `_camlStdlib$entry`, `_camlStdlib__Sys$entry` etc then
we finally call `_camlMeander$entry` which we saw earlier.

We need this assembly file to generate an object file for linking into the final executable.
If not the linker won't have `_caml_program` symbol available and none of the OCaml Stdlib will
be initialised. A fun exercise is to re-write this file to not call all those `entry` functions
but still provide `_caml_program` and call into `_camlMeander$entry`.

I made small [PR #13217](https://github.com/ocaml/ocaml/pull/13217) to improve this behaviour
to loop over a table of functions to call rather than generating large slabs of identical code.

## Bonus

Now you we don't need the OCaml compiler to write OCaml.

But seriously the purpose for discovering this was to investigate adding DWARF debugging
information to OCaml on macOS. That's a different topic for next time.