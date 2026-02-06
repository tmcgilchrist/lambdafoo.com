---
layout: post
title: "Why do frame pointers matter for OCaml?"
date: 2025-02-24 08:00
comments: false
categories: ocaml
---

At the end of last year at Tarides, my colleagues and I worked on improving the support for Frame Pointers in OCaml. While pitching the work internally, we were asked the question: **Why do frame pointers matter for OCaml?** In this post, I will share the response I gave.

We want to support frame pointers in the OCaml compiler because it gives us a reliable way to profile OCaml programs across different hardware using standard tooling like perf or eBFPF that people are already familiar with. Targeting all Tier-1 supported platforms (AMD64, ARM64, RISC-V, s390x and Power) would give a consistent experience across them all. By focusing on reusing standard tooling we can leverage the solutions built for other languages and get better impact for the smaller OCaml community, think reusing Instruments on macOS or the ecosystem of eBPF tools on Linux. An additional benefit is that frame pointers can be useful for source-level debuggers, as a last resort when debugging information and when symbols are absent.

# What are Frame Pointers?

The Frame Pointer (also known as the base pointer) is a register that points to the base of the current stack frame (e.g., `%rbp` on AMD64 or `x29` on ARM64). The stack frame (also known as the activation frame or the activation record) refers to the portion of the stack allocated to a single function call. This is where a function will save registers to or use for storage while performing work. By saving the frame pointer along with the return address, the call stack for OCaml can be calculated in a process called unwinding. As an example ARM64 call stack looks like this (note that the stack grows downwards to lower addresses):

``` assembly
          Stack                        Instruction Text     
   ┌────────────────────┐            ┌────────────────────┐ 
   │Return Address (x30)┼───────────►│ ancestor function  │ 
┌─►│Saved Frame Pointer │            │                    │ 
│  ┼────────────────────┼            │                    │ 
│  │Local allocations   │     ┌─────►│ parent function    │ 
│  │                    │     │      │                    │ 
│  ├────────────────────┤     │      │                    │ 
│  │Return Address (x30)├─────┼─────►│ grand-parent func  │ 
└──┤Saved Frame Pointer │     │      │                    │ 
┌─►┼────────────────────┼     │      │                    │ 
│  │Local allocations   │     │   ┌─►│ current function   │ 
│  │                    │     │   │  │                    │ 
│  ├────────────────────┤     │   │  └────────────────────┘ 
│  │Return Address (x30)├─────┘   │                         
└──┤Saved Frame Pointer │         │                         
   ┼────────────────────┼◄──┐     │      Registers          
   │Local allocations   │   │     │ ┌──────────────────────┐
   │                    │   └─────┼─┤ x29 - frame pointer  │
   └────────────────────┘◄──┐     │ │ x30 - link register  │
                            └─────┼─┤ sp  - stack pointer  │
                                  └─┼ pc  - program counter│
                                    └──────────────────────┘
```

More specifically, frame pointers can be used by profilers to unwind the stack in a language agnostic way. Many languages like C/C++, Rust, and Erlang support maintaining frame pointers.

# Return of the Frame Pointers

We started with the ambitious goal of adding frame pointer support to all the Tier-1 platforms supported by OCaml. That included the popular AMD64 and ARM64 platforms that cover most users, and less common platforms like RISC-V, Power and s390x.

The specific focus was to address the recognised limitations of perf when used with OCaml 5 programs ([#12563](https://github.com/ocaml/ocaml/issues/12563). OCaml 5 (aka multicore) introduced non-contiguous stacks as part of the implementation of effects. Mentioned in the PLDI 2021 paper on [retrofitting effect handlers -- Section 5.5](https://kcsrk.info/papers/retro-concurrency_pldi_21.pdf). At a high level, these stacks are stored in memory allocated by the runtime and not on the stack as would happen with C/C++. These non-contiguous stacks are essentially unknown to perf and will not work correctly with the copying nature of perf, it will copy the wrong things. So traces produced for OCaml 5 will appear truncated or contain incorrect values. This same problem occurs if you use DWARF call graphs, in particular perf will copy a chunk of the stack without decoding it via DWARF and for OCaml 5 onwards what it copies might not be stack. So the best solution is frame pointers.

We started by looking at [#11144](https:://github.com/ocaml/ocaml/pull/11144) which restored frame-pointers support for AMD64 after the initial OCaml 5.0 multicore release. It was clear that adding frame pointers required changes to both the backend assembly code generation and the OCaml runtime, and that the general design should follow the existing AMD64 approach. With that understanding the first step was to extend the `--enable-frame-pointers` autoconf file to allow configuring the compiler on macOS which only required a one line change of `[x86_64-*-linux*|x86_64-*-darwin*],` to recognise the new platform (more [context](https://github.com/tmcgilchrist/ocaml/blob/c1eec79948f699f2c9d8425c61bcc29553243bf1/configure.ac#L2458-L2472)).

Now, that allowed configuring the compiler with `./configure --enable-frame-pointers` and since the work had already been done to codegen and runtime, we only needed to test the changes on AMD64 macOS.

The next platform we chose was ARM64, for the dual reasons that it is a common platform for cloud vendors with most offering a Linux ARM64 option and that all new Apple laptops come with an ARM64 CPU (aka Apple Silicon). Getting ARM64 working seemed like it would have the most impact,, both for OCaml deployments and for local development where Apple laptops are quite common. Implementing again starting with a simple change to add new Linux and macOS cases to autoconf `[x86_64-*-linux*|x86_64-*-darwin*|aarch64-*-linux*|aarch64-*-darwin*],`. This exposes a configuration flag `Config.with_frame_pointers` that we can query when implementing the assembly code generation.

The assembly generation code is in the `asmcomp` directory of the [OCaml sources](https://github.com/ocaml/ocaml), the common code is in that directory and each platform supported has a sub-directory for it e.g. `asmcomp/arm64` for ARM64. Conceptually the changes required a modification to the calling convention for OCaml code to only use `x29` as a frame pointer (previously it was considered as a general purpose register), a second change to the calculation of stack frame sizes to allow for an extra register save, and finally emitting the correct assembly to save/restore `x29`.

The end result is that frame pointer save and restore is implemented using `stp` and `ldp` instructions plus an extra `add` instruction for updating `x29`. This results in an extra `add` plus saving an extra register for functions allocating a stack frame. In assembly this looks like:

``` asm
;; function prologue
	stp	x29, x30, [sp, #-16]
	sub	sp, sp, #16
	add	x29,  sp, #0

;; function epilogue
	add	sp, sp, #16
	ldp	x29, x30, [sp, #-16]
```

Whereas previously the store and load would just operate on the `x30` the link register. The overhead is quite minimal. The compiler already maintained the `x29` register when calling into C and when using TSan. Plus there is minimal extra stack space usage, as we needed to be quad word aligned anyway, so the `x29` register is going into an already allocated space on the stack. The end result is quite pleasing.

It's interesting to note that the [ABI on macOS](https://developer.apple.com/documentation/xcode/writing-arm64-code-for-apple-platforms) requires maintaining frame pointers, and certain popular Linux distributions like [Ubuntu](https://ubuntu.com/blog/ubuntu-performance-engineering-with-frame-pointers-by-default), [Fedora](https://pagure.io/fesco/issue/2923) and [Arch](https://gitlab.archlinux.org/archlinux/rfcs/-/merge_requests/26) are reenabling frame pointers in recent distributions. So I feel the impact on usability will be high and the runtime overhead quite low.

# What we achieved / What is left to do?

The short version is that frame pointers are now available for the two most popular Unix platforms (AMD64 and ARM64), that covers most deployments of OCaml and many OCaml developers. The work is split across these PRs:

 * [#13163](https://github.com/ocaml/ocaml/pull/13163): Enabled frame pointers on macOS x86_64 worked with some minor autoconf changes to correctly detect macOS and updating the tests to remove Linux specific backtrace formatting. Available in OCmal 5.3.

* [#13500](https://github.com/ocaml/ocaml/pull/13500): Added frame pointers support for ARM64 on Linux and macOS. This will be released in OCaml 5.4.

* [#13595](https://github.com/ocaml/ocaml/pull/13595): Fixed a bug introduced in #13050 where the wrong Canonical Frame Address (CFA) register was used for calls from OCaml into non-allocating C code. This appeared as incorrect backtraces in debuggers like GDB when setting a breakpoint inside the C code.

* [#13575](https://github.com/ocaml/ocaml/issues/13575), [#13635](https://github.com/ocaml/ocaml/pull/13635): Maintain OCaml frame pointers correctly even when using C libraries that do not support them, allowing mixing OCaml code with frame pointers with an operating system environment that might not have them.

* [#13751](https://github.com/ocaml/ocaml/pull/13751/): proposes a new section for the OCaml manual detailing how to use frame pointers with Linux perf highlighting what works and what does not work (DWARF stack unwinding). Hopefully this will get merged soon and close the documentation gap for using perf reliably with OCaml programs. Should be available in OCaml 5.4.

The original target of all Tier-1 platforms was an ambitious one, unfortunately we didn't manage to finish everything. The work for adding RISC-V frame pointer support was started but needs further work. The work in progress available at [tmcgilchrist/ocaml#22](https://github.com/tmcgilchrist/ocaml/pull/22). The work on [s390x](https://github.com/tmcgilchrist/ocaml/pull/24) and Power didn't get much further than diagramming stack frames and reading ABI documents. In particular the Power backend is also missing CFI support which makes debugging very difficult and really needs to be added first before tackling frame pointer support. Essentially you don't even have a working debugger while debugging any problems with your frame pointer changes.

Next steps are finding time or help with finishing off the remaining platforms, starting with Risc-V. If you'd like to help please get in touch. Then doing some comprehensive benchmarking for the ARM64 platform, my intuition is there is negligible overhead when adding frame pointers since it uses a load/store pair instruction, but firm results would be better.

If you've made it this far thanks for reading.