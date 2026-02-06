---
layout: post
title: "More CFI and frame pointers work"
date: 2026-02-07 08:00
comments: false
categories: ocaml
---

This month I set myself a goal to wrap up different pieces of work on OCaml that have been taking up space in my notes and my mind from last year. I wanted a clean slate to focus on the new year. I also enjoy reading other developers notes like [Opening up old release branches](https://www.dra27.uk/blog/platform/2026/01/16/dusting-off-the-branches.html) and [Faster Faster GDB Startup](https://tromey.com/blog/?p=1111). So here is my minor contribution and we are jumping straight in!

## What is CFI?

If you've ever tried to get a backtrace in GDB on Power and gotten garbage, that's because there was no CFI to help the debugger unwind the call stack. Call Frame Information (CFI) is a part of the DWARF standard, it's used for describing the call frames and register usage of a language. A call frame is a specific area of memory that is allocated on the stack, which typically corresponds to a function call in the source language. However some functions won't allocate a call frame like leaf functions or tail-recursive functions. I have some ASCII stack frame diagrams later.

Within each call frame, the CPU saves a set of registers, which ones depend on the architecture. When returning from a function, those registers need to be restored. The ABI document spells out the details; for Power that's the 64-Bit ELF V2 ABI Specification.

The code that allocates space on the call frame stack and performs the save operations is called the function prologue, and the corresponding code that performs the restore operation and deallocates the frame is called the epilogue.

The debugger uses CFI to:

 1. Generate a backtrace by _unwinding_ the function call stack.
 2. Restore the state of registers when visiting previous functions higher up in the call stack.
 3. Find registers that get spilled to the stack when they get overwritten within a function.

For our purposes, we care about working backtraces and recovering key registers like the return address and frame pointer.

The DWARF specification introduces the term CFA (Canonical Frame Address), is a specific point in the stack frame that is used as the base address for finding everything else in the frame. For example, the Link Register representing the address to return to when exiting a function is stored at CFA+16 on Power. With this terminology out of the way let's look at how to apply it to Power (if you want more details checkout the excellent book [Building a Debugger](https://nostarch.com/building-a-debugger)).

### Power CFI

When Power architecture support was added back into OCaml 5 in [ocaml#12276](https://github.com/ocaml/ocaml/pull/12276) there was no support for Call Frame Information. Without CFI it is difficult to debug problems on Power and do performance engineering as you don't have working backtraces. So last year I started work on adding DWARF Call Frame Information (CFI) directives to the Power backend so that debuggers like GDB can unwind the stack through OCaml frames, C-to-OCaml transitions, and runtime functions (GC, C calls, exceptions, effects etc).

For the Power architecture, the calling conventions are defined in the [64-Bit ELF V2 ABI Specification: Power Architecture](https://openpowerfoundation.org/specifications/64bitelfabi/) document and specifically we're interested in the section _2.2.2. The Stack Frame_ which details the calling conventions for C. What registers are used? Which registers should be saved between function calls? What do those registers represent (return addresses or stack pointers)?

For 64-Bit ELF V2 ABI this looks like:
```

                                  Higher addresses
                             ┌─────────────────────────────┐
     Caller's SP  ─────────► │  Back-chain (caller's)      │  8 bytes
                             ├─────────────────────────────┤
                             │  Floating-Point Register    │  8 × (32 − N) bytes
                             │  Save Area                  │  fN saved at
                             │  (optional, callee saves)   │  caller_SP − 8×(32−N)
                             ├─────────────────────────────┤
                             │  General-Purpose Register   │  8 × (32 − N) bytes
                             │  Save Area                  │  rN saved at
                             │  (optional, callee saves)   │  (below FPR save area)
                             ├ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ┤
                             │  Alignment padding          │  0 or 8 bytes
                             │  (for quadword alignment)   │  (if needed)
                             ├─────────────────────────────┤
                             │ Vector Register             │  16 × (32 − N) bytes
                             │ Save Area                   │  vN saved at
                             │ (optional, quadword aligned)│  (below GPR area + pad)
                             ├─────────────────────────────┤
                             │  Local Variable Space       │  variable
                             │  (optional)                 │
                             ├─────────────────────────────┤
     SP + 32      ─────────► │  Parameter Save Area        │  variable (optional)
                             │  (optional, 8 × num params) │  (quadword aligned)
                             ├─────────────────────────────┤
     SP + 24      ─────────► │  TOC Save (r2)              │  8 bytes
                             ├─────────────────────────────┤
     SP + 16      ─────────► │  LR Save                    │  8 bytes
                             ├─────────────────────────────┤
     SP + 8       ─────────► │  CR Save (word) + Reserved  │  4 + 4 bytes
                             ├─────────────────────────────┤
     SP           ─────────► │  Back-chain                 │  8 bytes
                             └─────────────────────────────┘
                                  Lower addresses

```

> The minimum stack frame size shall be 32 bytes. A minimum stack frame consists of the first 4 doublewords (back-chain doubleword, CR save word and reserved word, LR save doubleword, and TOC pointer doubleword), with padding to meet the 16-byte alignment requirement.

For OCaml we retain this 32 byte structure for compatibility with C and layer OCaml specific structure on top. Here are the interesting bits:

 1. *LR is always at CFA+16*. OCaml uses the Power ELF V2 ABI which mandates the caller reserves SP+16 for the link register save. So we use CFI to declare this fact using `.cfi_offset 65, 16`.
 2. *Function prologue instructions reordered*. The old prologue adjusted the stack pointer register (SP) first (*addi*), then saved LR. The new prologue saves LR/TOC into the caller's frame first, then adjusts SP with *stdu* (store-with-update, which atomically writes the back-chain pointer). This more closely matches the Power ABI convention that C compilers follow and is *probably* better for async-signal safety. The Table of Contents (or TOC) is a pointer used on Power for position-independent code support.
 3. *cfi_startproc moved before instructions*. This assembler directive tells the debugger where a function begins, previously it was added after the TOC setup and debug information. Now it's directly after the symbol label so the unwinder covers the full function body and we avoid certain edgecases when setting breakpoints.
 4. *DWARF expressions for stack switches*. When switching between the OCaml stack and C stack (and the opposite direction), a moderately complex *.cfi_escape* expressions computes the CFA by dereferencing the saved stack pointer from the *c_stack_link* structure. This was the bulk of the work, understanding the structure of the C stack frames to load the right saved registers. Unfortunately there aren't many tools to debug when you get this wrong, I ended up writing Python scripts to print out chunks of memory and work out which bits pointed to code or other stack frames. There is a project in here somewhere to run the stack machine for CFI and validating it against memory that I might get to later this year.
 5. *Signal frames*. Runtime entry points (caml_call_gc, caml_c_call, caml_call_realloc_stack, etc.) are marked with *.cfi_signal_frame* so the unwinder treats them as asynchronous interruption points, prints out "\<signal handler called\>" and avoids some stack integrity checks that cause GDB to stop unwinding.
 6. *Pre-initialized LR save area*. In caml_start_program, before calling into OCaml, the code stores the return address at SP+16 so the unwinder can find LR even if the first  OCaml function has fun_frame_required = false. This was needed for exception handling code.

Here's a nice ASCII diagram of an OCaml Frame on Power:
```

                           Higher addresses
                      ┌─────────────────────────┐
        CFA+16  ───►  │  LR save (return addr)  │  8 bytes ◄── always at caller_SP+16
                      ├─────────────────────────┤
        CFA+8   ───►  │  TOC save (r2)          │  8 bytes ◄── always at caller_SP+8
                      ├─────────────────────────┤
        CFA     ───►  │                         │
                      ╞═════════════════════════╡ ◄── SP + frame_size  (= CFA)
                      │                         │
                      │  Reserved stack space   │  32 bytes
                      │  (ABI-mandated)         │
                      ├─────────────────────────┤
                      │  Float stack slots      │  num_float_slots × 8
                      ├─────────────────────────┤
                      │  Int stack slots        │  num_int_slots × 8
                      ├─────────────────────────┤
                      │  (stack_offset area)    │  trap frames (16)
                      │                         │  outgoing params, etc.
                      ├─────────────────────────┤
          SP    ───►  │  (16-byte aligned)      │
                      └─────────────────────────┘
                           Lower addresses
```

Now we have working backtraces on Power for GDB. With working CFI in place, the next step was adding frame pointer support.

### More frame pointers

I wrote about the motivation for frame pointers in [Why do frame pointers matter for OCaml?](https://lambdafoo.com/posts/2025-02-24-ocaml-frame-pointers.html). At the end I mentioned I didn't have working code for RISC-V, Power or s390x. This is the next chunk of work getting frame pointers for the full set of architectures.

#### Power Frame Pointers

Power frame pointer support built on top of the earlier CFI work and adds save/restore instructions for maintaining r31 (the frame pointer register). Power ABI documents use the term *back-chain* to describe the saved frame pointer for the previous frame, which is also used in s390x documents later on. The main changes were:

  1. *Allocation pointer moved from r31 to r23*. The GC allocation pointer previously lived in r31, which is the standard Power ABI frame pointer register. Moving it to r23 (another callee-saved register) freed up r31 for its intended purpose.
  2. *Back-chain maintained for every frame*. Every frame allocation now uses stdu (store-with-update) instead of addi to adjust the stack pointer, which atomically writes the back-chain. This required some recalculation of instruction sizes to ensure we always emit valid branch instructions.
  3. *Trap frame was enlarged from 16 to 48 bytes*. This was the surprise! Trap frames are used by the OCaml runtime for handling exceptions and effects. The old 16-byte trap frame placed trap data at SP+32 and SP+40 (within the reserved area). With stdu-based allocation, the back-chain at new_SP points to old_SP, and the ABI expects old_SP+16 to hold LR. At trap_size=16, old_SP+16 = new_SP+48, which falls outside the allocated region. Increasing to 48 bytes ensures the LR save area at back_chain+16 lies within the trap frame, and trap data at new_SP+32/new_SP+40 doesn't collide with the caller's save area.
  4. *Runtime frame pointer fixups*. When the OCaml stack grows, the runtime needs to walk the stack and rewrite the saved FP values so the frame pointer chain remains valid.

Now an OCaml Frame with frame pointers looks like this:

```
                               Higher addresses
                          ┌─────────────────────────┐
            CFA+16  ───►  │  LR save (return addr)  │  8    ◄── ABI: caller_SP+16
                          ├─────────────────────────┤
            CFA+8   ───►  │  TOC save (r2)          │  8    ◄── ABI: caller_SP+8
                          ├─────────────────────────┤
            CFA     ───►  │  Back-chain (prev SP)   │  8    ◄── written by stdu
            ══════════════╪═════════════════════════╪══════════ FP after prologue
                          │                         │
                          │  Reserved stack space   │  32 bytes (ABI-mandated)
                          ├─────────────────────────┤
                          │  Float stack slots      │  num_float_slots × 8
                          ├─────────────────────────┤
                          │  Int stack slots        │  num_int_slots × 8
                          ├─────────────────────────┤
     fp_save_offset ───►  │  Saved r31 (old FP)     │  8    ◄── FP-only
                          ├─────────────────────────┤
                          │  (stack_offset area)    │  trap frames, outgoing
                          │                         │  params, etc.
                          ├─────────────────────────┤
            SP = FP ───►  │  (16-byte aligned)      │
                          └─────────────────────────┘
                               Lower addresses
```

This change was fairly painful because I didn't realise I needed to increase the trap size and ran into issues with instructions trashing saved registers on the stack. GDB support for watching memory addresses and breaking when something wrote to that address was really useful here. Go read the [ocaml#14482](https://github.com/ocaml/ocaml/pull/14482) for more details.

#### RISC-V Frame Pointers

RISC-V frame pointers was started by Miod Vallat in 2024, who had half the tests passing before I picked this up again. The surprising thing about RISC-V is that the frame pointer points to CFA, *above the saved registers*, rather than to the frame record itself like ARM64 does. The RISC-V ABI convention is s0 = sp + frame_size, pointing to the slot just past the *{saved_s0, saved_ra}* pair at the top of the frame.

Comparing this to ARM64 makes the difference more obvious (note x29 is the ARM64 frame pointer register).

```
    RISC-V                              ARM64
    ──────                              ─────
         ┌───────────┐                       ┌───────────┐
    s0 ──▶  CFA      │                       │  CFA      │
         ├───────────┤                       ├───────────┤
    -8   │  ra       │                  -8   │  x30 (lr) │
         ├───────────┤                       ├───────────┤
    -16  │  old s0   │            x29 ──▶-16 │  old x29  │
         ├───────────┤                       ├───────────┤
         │  locals   │                       │  locals   │
         └───────────┘                       └───────────┘

    s0 = CFA                           x29 = CFA - 16
    s0 = sp + frame_size               x29 = sp + frame_size - 16
```

The main pain-point after working this out was adjusting the frame walking code in the runtime and adjusting the runtime assembly stubs. RISC-V frame pointers point to CFA (above the frame record), not to the frame record itself. When *caml_try_realloc_stack* rewrites frame pointers after growing a fiber stack (OCaml 5 features effect handlers which get implemented as fiber stacks in the runtime), it must account for this: fp->prev is a CFA value, and the next frame record is at fp->prev - 1 (i.e. CFA − 16 bytes).

This ticked off another architecture supporting frame pointers and with that the Tier-1 unsupported architectures list got shorter. [ocaml#14506](https://github.com/ocaml/ocaml/pull/14506) has the full code.

#### s390x Frame Pointers

Now, s390x frame pointers seemed like they would be straightforward. I had implemented other platforms like Power and ARM64, and had a good understanding of which areas I needed to modify:

1. enable the configure flag,
2. update the code emission logic for frame sizes,
3. fix up the prologue/epilogue assembly for back-chain handling,
4. add CFI directives, handle effect stack switching, and
5. update the runtime's assembly stubs.

The usual drill, I even had it hand-written on paper. What I didn't account for is the difference in stack layout, which can be reduced to OCaml wants to store the back-chain and r14 at SP+0 and SP+8 respectively, while s390x ABI mandates back-chain at SP+0 and r14 at SP+112. The s390x ABI also requires a minimum of 160 bytes for the frame size. I'll write about this another time but the result is I'm still working out what approach to take (the code is at [tmcgilchrist/ocaml#24](https://github.com/tmcgilchrist/ocaml/pull/24)). All the options seem terrible right now.

### Smaller fixes

While I was working in this area there were some smaller fixes to be made. Recall from earlier we need to write *.cfi_escape* expressions to describe how to find the C stack from OCaml (and the reverse). The expressions for s390x were slightly wrong and needed some tweaking; [ocaml#14500](https://github.com/ocaml/ocaml/pull/14500) has more details. Once I had stack diagrams for s390x the work mostly involved writing Python to inspect memory and adjusting where the CFI expressions pointed to in memory.

FreeBSD frame pointers was requested during ICFP 2025 and I thought it would be a simple matter of changing the configure script to allow it. Nothing is ever simple. The frame pointer tests were failing because the first symbol in a module would clash with the *module.entry* symbol (representing the module initialisation code associated with every module in OCaml e.g. camlFoo.entry) breaking the frame pointer tests. Once I re-discovered that detail ([ocaml#4690](https://github.com/ocaml/ocaml/issues/4690)), the fix was simple. This seems to be a common problem for LLVM toolchains. For good measure I documented how to use *dtrace* and *pmcstat* to do CPU profiling, see [ocaml#14486](https://github.com/ocaml/ocaml/pull/14486).

## Conclusion

With all that OCaml now has working debugger support on Power, completing the set of Tier-1 architectures where such tools will work. This exceeds the support that existed in OCaml prior to 5.0 and completes the one thorny issue I knew about to achieve feature parity. Closing the CFI s390x bug I reported in 2024 resolved the last known issue with debuggers on that platform.

On top of that we have frame pointer support for more architectures than ever, if you're debugging or profiling OCaml on Power, RISC-V, ARM64, or AMD64, you now get clean backtraces. As I wrote in the OCaml manual [section on profiling](https://ocaml.org/manual/5.4/profil.html#s:ocamlprof-time-profiling) this is key to getting accurate profiling using Linux perf and macOS Instruments. FreeBSD users can profile with dtrace and pmcstat, and there is documentation on how to get started.

All these changes should eventually reach an OCaml release, if I'm lucky the smaller bug fixes will squeeze into 5.6 and the rest targeting 5.7. That leaves s390x frame pointers (still wrestling with the 160-byte minimum frame problem) and Windows (the elephant in the room). We shall see.
