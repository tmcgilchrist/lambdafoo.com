---
layout: post
title: "Experimenting with OCaml and eBPF"
date: 2025-02-14 08:00
comments: false
categories: ocaml, ebpf
published: true
---

Building on top of the excellent book _BPF Performance Tools_ by Brendan Gregg. How can we apply the techniques from Chapter 12 Languages to OCaml?

First OCaml is roughly equivalent to C, it's a compiled language with a runtime written in C. It supports frame pointers using the `--enable-frame-pointers` configuration option on x86_64, with ARM64 support in OCaml 5.4. Eventually the code we're interested in is C or looks roughly like C but with a different calling convention.

For tracing into the Linux kernel, you'll need a distribution that is compiled with frame pointers like Ubuntu 24.04 and we can reuse the kernel's own symbol table. There are some exceptions for inlined functions and some blacklisted functions that aren't safe to trace. However for the pieces I've looked at like memory allocation and virtual memory, it is fine.

For the OCaml runtime written in C, it can be configured to include symbols, frame pointers and debuginfo for the portions written in C. The sections of the runtime written in assembly have symbols and frame pointers. For actual OCaml code it will have symbols and frame pointers, with limited debuginfo. Demo time!

# OCaml Function Tracing

Given this test program taken from a bug report [#13123](https://github.com/ocaml/ocaml/issues/13123) against OCaml.

``` ocaml
(* Build with:
  ocamlfind ocamlopt -package unix -package threads -thread -linkpkg -o liquidsoap_test.exe liquidsoap_test.ml *)
let frame_size = 0.04
let pcm_len = int_of_float (44100. *. frame_size)
let channels = 2

let mk_pcm () = Array.init channels (fun _ -> Array.make pcm_len 0.)

let rec fn a =
  if Array.length a <> 0 then
    Gc.full_major ();
  let pcm = mk_pcm () in
  ignore(pcm);
  Unix.sleepf 0.04;
  fn [||]

let () =
  let deadweigth = Array.make (40 * 1024 * 1024) 1 in
  Unix.sleepf 0.04;
  let th = Thread.create fn deadweigth in
  Thread.join th
```

And an opam switch created with frame pointers.

``` shell
$ opam switch create 5.3.0 5.3.0+options ocaml-option-fp

$ ocamlfind ocamlopt -package unix -package threads -thread \
  -linkpkg -o liquidsoap_test.exe liquidsoap_test.ml
```
Running this code will print the PID each time the `liquidsoap_test.exe` executable is run.

``` shell
$ sudo bpftrace -e 'uprobe:/home/tsmc/ocaml-performance/liquidsoap_test.exe:caml_start_program { printf("OCaml run with process ID %d\n", pid); }'
```

Here we are using the information we know about OCaml startup, the `caml_start_program` is an assembly function that bridges the gap between the C startup code and OCaml, setting up the environment for the OCaml code. The section after `uprobe:` needs to point to the executable being run, change that if you want to trace something else.

Next, recall that we are dealing with a mix of regular C functions and OCaml functions. Listing the tracepoints available shows a mix of regular C functions prefixed with `caml_*` that are either part of the runtime or C primitives. OCaml compiler performs name mangling so anything coming from an OCaml source file will have a prefix `caml<MODULE>.` e.g. `camlStdlib__Domain*`  for the `domain.ml` module from the standard library or `camlStdlib__Int.compare_296` for the compare function on Int. Armed with that knowledge. This command will list available probe points:
``` shell
$ sudo bpftrace -l 'uprobe:/home/tsmc/ocaml-performance/liquidsoap_test.exe:*'
```

If we wanted to count the number of function calls in a binary, we could do it like so:
``` shell
$ cat count.bt
# Printout matched program
uprobe:/home/tsmc/ocaml-performance/liquidsoap_test.exe:caml_start_program
{
  printf("OCaml run with process ID %d\n", pid);
}

# Trace function calls
uprobe:/home/tsmc/ocaml-performance/liquidsoap_test.exe:camlLiquidsoap_test*
{
    @[probe] = count();
}

$ sudo bpftrace count.bt
Attaching 5 probes...
OCaml run with process ID 128477
^C

@[uprobe:/home/tsmc/ocaml-performance/liquidsoap_test.exe:camlLiquidsoap_test.entry]: 1
@[uprobe:/home/tsmc/ocaml-performance/liquidsoap_test.exe:camlLiquidsoap_test.fn_327]: 1
@[uprobe:/home/tsmc/ocaml-performance/liquidsoap_test.exe:camlLiquidsoap_test.mk_pcm_273]: 1029
@[uprobe:/home/tsmc/ocaml-performance/liquidsoap_test.exe:camlLiquidsoap_test.fun_601]: 2058
```

Another thing we could do is see how much time is spent in the minor GC promotion function.

OCaml uses a bump-pointer allocator for the minor heap, when that is full it will call a C function to scan the minor heap, destroy the junk, and promote anything that survives into the major heap. I know that the main entry point for this is called `caml_empty_minor_heap_promote`. So this script will instrument the entry and exit for that function and print out a histogram of the time taken.

``` shell
# cat gcprofile.bt

uprobe:/home/tsmc/ocaml-performance/liquidsoap_test.exe:caml_start_program
{
  printf("Attaching to OCaml process ID %d\n", pid);
}

uprobe:/home/tsmc/ocaml-performance/liquidsoap_test.exe:caml_empty_minor_heap_promote
{
  @t = nsecs;
}

uretprobe:/home/tsmc/ocaml-performance/liquidsoap_test.exe:caml_empty_minor_heap_promote / @t /
{
  @minor_gc_times = hist(nsecs - @t);
}
```

What about the major GC? The design of that is more complicated but I know `major_collection_slice` does the majority of the work, so we attach there.

``` shell
# cat gcprofile_major.bt

uprobe:/home/tsmc/ocaml-performance/liquidsoap_test.exe:caml_start_program
{
  printf("Attaching to OCaml process ID %d\n", pid);
}

uprobe:/home/tsmc/ocaml-performance/liquidsoap_test.exe:major_collection_slice
{
  @t = nsecs;
}

uretprobe:/home/tsmc/ocaml-performance/liquidsoap_test.exe:major_collection_slice / @t /
{
  @major_gc_slice_times = hist(nsecs - @t);
}
```

# Take Away

OCaml programs can traced with eBPF and bpftrace. You need to install OCaml with frame pointers enabled and use a Linux distribution like Ubuntu 24.04 that also enables frame pointers, so you can trace into system libraries. The OCaml runtime and certain primitives use a symbol prefix of `caml_` and OCaml code uses a prefix of `caml<MODULE>` where `<MODULE>` is the OCaml module containing the code. This partially covers the functionality in [ocamlprof](https://ocaml.org/manual/5.3/profil.html) which lets you profile function counts and branches taken in things like while, if and try. With eBPF we can count the function calls but more work needs to be done to support the branching constructs, essentially we need a USDT implementation for OCaml that understands OCaml's name mangling and calling conventions. The upside is eBPF can be applied to any OCaml binary without needing a recompile.

Next steps are adding USDT probes to the OCaml runtime, so there is a static API for the GC, and after that expose USDT probe points from OCaml programs.