---
layout: post
title: "Quick Hardware Performance Counters on macOS ARM64"
date: 2026-03-25 08:00
comments: false
categories: ocaml, profiling, macos
---

If you've ever profiled OCaml programs on Linux, you've probably reached for `perf stat`. It's the go-to tool for grabbing hardware performance counters—cycles, instructions, cache misses—without any instrumentation overhead. On macOS, the equivalent story has been **open Instruments**, which is fine for GUI-driven investigation but terrible for automated benchmarking pipelines.

I wanted something I could stick in a shell script, get output as JSON, and run in the terminal. So I put together [mperf](https://github.com/tmcgilchrist/mperf), a `perf stat`-like CLI for Apple Silicon. Here is what it looks like:

```
$ sudo ./mperf-stat -e cycles -e instructions -e l1d-tlb-misses -- ./my_benchmark

 Performance counter stats:

         1,234,567,890  cycles
         2,345,678,901  instructions                # 1.90 IPC
            12,345,678  l1d-tlb-misses

       0.543210 seconds wall time
       0.520000 seconds user
       0.020000 seconds sys
```

## Why Not Just Use Instruments?

Instruments is powerful, but it's an interactive GUI tool. You can invoke it from the command line using `xctrace` but the results need the same GUI tool to view them. Sometimes you just need a simple cli tool that prints out the most interesting stats, in my case I want it invoked from a Makefile or a CI runner.

There's also no good reason this should require full Xcode and Instruments. The hardware counters are right there in the CPU; the kernel exposes them through private frameworks. The only real requirement is root access, no need for disabling SIP or code signing or other special entitlements.

## Using Apple's Private Frameworks

Apple Silicon exposes hardware performance counters through two private frameworks: `kperf.framework` and `kperfdata.framework`, living under `/System/Library/PrivateFrameworks/`. These are the same frameworks that Instruments uses internally. They're undocumented, but [ibireme's kpc_demo](https://gist.github.com/ibireme/173517c208c7dc333ba962c1f0d67d12) showed that you can load them at runtime with `dlsym` and drive them from userspace.

The CPU-specific event databases live in `/usr/share/kpep/` as plist files—`a14.plist` for M1, `a15.plist` for M2, `as4.plist` for M4, and so on. mperf provides portable aliases (`cycles`, `instructions`, `branch-misses`, `l1d-cache-misses`, etc.) that resolve to the right event names for whatever chip you're running on. You can also pass raw event names if you want something specific.

Apple Silicon gives you 2 fixed counters (cycles and instructions) plus 8 configurable counters, for a maximum of 10 simultaneous events. Unlike Linux perf, mperf doesn't do multiplexing — if you ask for more than 10 events, it's an error rather than a silently degraded estimate. More on that distinction below.

## The Multi-Threading Problem

A simple approach would be to fork a child, start counting, wait for it to exit, read counters. That works for single-threaded programs, but OCaml 5.x programs with multiple domains spawn multiple pthreads—each domain gets a domain thread plus a backup thread for systhreads. A 4-domain program has at least 8 pthreads, and naive per-thread measurement would miss most of the work.

This is where Apple's **Profile Every Thread (PET)** mechanism comes in. Instead of reading counters for a single thread, PET sets up a kernel timer that fires periodically (default: every 1ms) and snapshots PMC values for _every_ thread matching a PID filter. These samples get written to a kernel trace buffer (`kdebug`) with thread IDs and timestamps.

The approach is:

 1. Fork a child process, held at a pipe barrier
 2. Configure the PMC hardware with requested events
 3. Set up PET sampling filtered to the child's PID
 4. Enable kdebug tracing for `PERF_KPC_DATA_THREAD` events
 5. Release the child (close the pipe), let it exec the target command
 6. Poll kdebug for samples until the child exits
 7. For each thread, compute the delta between first and last sample
 8. Sum deltas across all threads

```
Thread 1: [sample_0] -------- [sample_1] -------- [sample_N]
Thread 2:      [sample_0] -------- [sample_1] -------- [sample_N]
Thread 3:           [sample_0] -------- [sample_1] -------- [sample_N]
...

Result = Σ (thread_last - thread_first) for all threads
```

This is fundamentally a sampling-based approximation rather than continuous counting. But for benchmarks that run longer than a few milliseconds, the results are accurate enough to be useful. The comparison with Linux `perf stat` is more nuanced than "exact vs approximate" though.

## Sampling Period Trade-offs

The `-p` flag controls the sampling period. The default 1ms works well for most OCaml programs since domains typically live for the program's duration. For short-lived benchmarks you can go faster at the cost of more overhead by setting smaller values for `-p`.

```bash
# Default 1ms - good balance for most programs
sudo ./mperf-stat -e cycles -e instructions -- ./benchmark

# 0.5ms - catches short-lived threads, higher overhead
sudo ./mperf-stat -p 0.5 -e cycles -e instructions -- ./short_benchmark

# 5ms - less overhead for long-running workloads
sudo ./mperf-stat -p 5 -e cycles -e instructions -- ./long_benchmark
```

## OCaml Bindings

Since the goal is integration with OCaml benchmarking services, mperf includes an OCaml library that wraps the CLI tool and parses its JSON output:

```ocaml
open Apple_perf_stat

let () =
  match run
    ~events:["cycles"; "instructions"; "l1d-tlb-misses"; "branch-misses"]
    ["./my_benchmark"; "--size"; "10000"]
  with
  | Ok result ->
    Printf.printf "IPC: %.2f\n" (get_ipc result);
    Printf.printf "Wall time: %.3f s\n" (wall_time_seconds result);
    (match get_counter "l1d-tlb-misses" result with
     | Some misses -> Printf.printf "TLB misses: %Ld\n" misses
     | None -> ())
  | Error e ->
    Printf.eprintf "Error: %s\n" (string_of_error e);
    exit 1
```

The library has no external dependencies beyond `unix` and `str` from the OCaml standard library. It handles the root privilege check, argument construction, JSON parsing, and error reporting.

The idea is to use instruction counts as a stable performance metric. Wall-clock time varies with system load and thermal throttling; instruction counts are deterministic. Pairing that with IPC and cache miss rates gives you a pretty complete picture of where regressions come from. The limit of 10 counters on Apple Silicon is enough to get started.

## Counting Accuracy

How accurate are the results from mperf? And how do they compare to Linux perf? 

It's tempting to describe this as "perf stat is exact, mperf is approximate" but that's not the full picture. `perf stat` uses exact hardware counting _only when the requested events fit within the CPU's physical PMU counters_. Once you exceed that limit, the kernel enables time-division multiplexing: it rotates events through the available counters via an `hrtimer` and scales the final counts:

```
final_count = raw_count * (time_enabled / time_running)
```

The [perf wiki](https://perfwiki.github.io/main/tutorial/) is explicit about this: "It is very important to understand this is an estimate not an actual count. Depending on the workload, there will be blind spots which can introduce errors during scaling." When multiplexing is active, `perf stat` shows a percentage column indicating what fraction of time each event was actually measured — anything below 100% means the count was extrapolated.

How many events before multiplexing kicks in depends on the hardware? It depends on how many PMU counters are exposed on the particular CPU. Here are a few examples from modern CPUs:

| CPU                          | General-Purpose | Fixed | Total |
|------------------------------|-----------------|-------|-------|
| Intel Skylake (HT on)        | 4               | 3     | 7     |
| Intel Ice Lake (HT off)      | 8               | 4     | 12    |
| AMD Zen 3/4                  | 6               | 0     | 6     |
| ARM Cortex-A76 / Neoverse N1 | 6               | 1     | 7     |
| Apple M1-M4                  | 8               | 2     | 10    |

On Linux you can check your own CPU with `dmesg`:

```bash
$ dmesg | grep -E "generic registers|fixed-purpose events"
... generic registers:      4
... fixed-purpose events:   3
```

Note that the NMI watchdog steals one general-purpose counter by default on most distros (`cat /proc/sys/kernel/nmi_watchdog` — if it returns 1, you've lost a counter).

So in practice the comparison is:

- **perf stat with ~6 events on Skylake**: exact counting, genuinely better than mperf's PET approach.
- **perf stat with 12+ events**: time-multiplexed scaled estimates. Still different from PET sampling (perf counts every event while the counter is active, then extrapolates for inactive periods), but the result is still an estimate.
- **mperf with up to 10 events**: PET sampling-based approximation, but all counters are always physically active: no time-sharing, no scaling. The inaccuracy comes from sampling granularity (missing activity between snapshots), not from extrapolation.

For the common case of measuring cycles + instructions + a handful of cache/TLB events (say 4-6 total), perf stat on Linux gives you exact counts and mperf gives you a close approximation. For larger event sets in perf, you hit the multiplexing case and might see unexpected results. Both tools are producing estimates through different mechanisms. mperf's hard limit of 10 events means you always know whether your counts are real.

The other caveats: mperf relies on Apple's private `kperf` and `kperfdata` frameworks, which could change with any macOS update (though they've been stable across M1 through M4). And very short-lived threads might be missed entirely if they complete within a single sampling period.

## Getting Started

```bash
git clone https://github.com/tmcgilchrist/mperf
cd mperf
make
sudo ./mperf-stat -- echo hello
```

List all available events for your CPU with `./mperf-stat -l`.

The code is based on [ibireme's kpc_demo.c](https://gist.github.com/ibireme/173517c208c7dc333ba962c1f0d67d12), with event documentation from [jiegec/apple-pmu](https://github.com/jiegec/apple-pmu) and the [Apple Silicon CPU Optimization Guide](https://developer.apple.com/documentation/apple-silicon/cpu-optimization-guide).

## What's Next

Porting this to x86_64 macOS, the PMC infrastructure should be similar. The more immediate goal is integrating mperf into an automated benchmarking pipeline for the OCaml compiler, tracking instruction counts and IPC across commits the way we've been doing with [frame pointers](https://lambdafoo.com/posts/2025-02-24-ocaml-frame-pointers.html) and [eBPF](https://lambdafoo.com/posts/2025-02-15-ocaml-ebpf-usdt.html) on Linux.

This fits into the broader effort of making OCaml programs easier to profile. Frame pointer support gives us `perf` and eBPF integration on Linux; mperf gives us hardware counter access on macOS without reaching for Instruments. Using frame pointers on macOS gives us accurate backtraces in Instruments when used with `xctrace`. 
