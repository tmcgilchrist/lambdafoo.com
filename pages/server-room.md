---
layout: page
title: "Server Room"
date: 2025-01-29 12:00
comments: false
sharing: false
footer: true
---

Documenting the server room at home that get used for working on OCaml and other open source software. Plus some general tinkering and running the lowest latency home network that "r/homelab" would love. 

## monolith

|          |                   |
|----------|-------------------|
| **CPU**  | AMD Ryzen 9 9950X |
| **RAM**  | 64 GB             |
| **Disk** | 1.8 TB            |
| **OS**   | Ubuntu 25.10      |
| **Arch** | x86_64            |

Primary Linux development server and workhorse for large compilations. You should see how fast it builds llvm-project and GHC. This is my most recent addition, built from scratch over Christmas 2025 before RAM prices went crazy.

## obelisk

|          |                                     |
|----------|-------------------------------------|
| **CPU**  | Intel Xeon E5-2667 v2 (Mac Pro 6,1) |
| **RAM**  | 64 GB                               |
| **OS**   | Ubuntu 24.04                        |
| **Arch** | x86_64                              |

Secondary x86_64 build machine repurposed from an old Mac Pro that I picked up on ebay for a few hundred dollars. Performs very credibly for a 12 year old machine, I've upgraded the CPU to an E5-2667 V2 to get the most CPU speed possible. Swapping CPUs was pretty scary, if I get the urge I have a 12 core E5-2697 V2.

## artemis

|          |              |
|----------|--------------|
| **CPU**  | Apple M3 Pro |
| **RAM**  | 36 GB        |
| **OS**   | macOS 15.7.3 |
| **Arch** | arm64        |

This is my go everywhere laptop for development and remoting into different Linux environments. The hardware is great and ARM64 CPU is fun.

## noir

|          |                                |
|----------|--------------------------------|
| **CPU**  | Intel i5-8500B (Mac Mini 2018) |
| **RAM**  | 64 GB                          |
| **OS**   | macOS 15.7.3                   |
| **Arch** | x86_64                         |

macOS x86_64 machine for testing OCaml support and running Windows 11 inside a VM.

## mirage

|          |                        |
|----------|------------------------|
| **CPU**  | Raspberry Pi 4 Model B |
| **RAM**  | 4 GB                   |
| **OS**   | Ubuntu 24.04           |
| **Arch** | aarch64                |

Mostly used as a SSH jump box and running small network applications.

## puck

|          |                                |
|----------|--------------------------------|
| **CPU**  | 7447A @ 1.5 GHz (PowerMac10,2) |
| **RAM**  | 491 MB                         |
| **OS**   | Debian GNU/Linux bookworm/sid  |
| **Arch** | ppc                            |

Used for testing Debian powerpc packages, and nostalgia for when PowerPC Macs and assembly was something I used. Slowly Linux is dropping powerpc support so it gets harder to justify turning this machine on. Originally this was the SSH jumpbox but I wasn't confident about getting security updates for Debian powerpc.

## penelope

|          |                                                        |
|----------|--------------------------------------------------------|
| **CPU**  | 2x Intel Xeon X5650 (12 cores @ 2.4 GHz, Mac Pro 2010) |
| **RAM**  | 32 GB                                                  |
| **OS**   | —                                                      |
| **Arch** | x86_64                                                 |

Currently offline, waiting for a replacement power supply and CPU upgrade. It will probably run Linux or FreeBSD when it's repaired.

## deimos

|          |                                                                |
|----------|----------------------------------------------------------------|
| **CPU**  | Intel Core i7 @ 3.0 GHz (MacBook Pro Retina 13-inch, Mid 2014) |
| **RAM**  | 16 GB                                                          |
| **OS**   | —                                                              |
| **Arch** | x86_64                                                         |

This is my old-old laptop and is currently unassigned. Candidate for a FreeBSD or OpenBSD install, when I get around to it

---

## Former Residents

Machines that have passed through the server room over the years.

- **SGI O2** — MIPS workstation, acquired from a friend. Lovely machine for running IRIX and just looking at.
- **SGI Indy** — Another MIPS box from the same friend, also ran IRIX. Actually I had a person sized stack of these from a company in Sydney, a mix of R5000 and R4400 CPUs.
- **Sun SparcStation 20** — Dual HyperSparc @ 180 MHz with a matching Sun 19 inch CRT and Sun keyboard/mouse. Ran Solaris and made plenty a lot of noise doing it.
- **Sun Ultra 5** — UltraSPARC IIi @ 360 MHz with 512 MB RAM. Was my main Solaris desktop along with a 19 inch CRT.
- **Sun Ultra 10** — UltraSPARC IIi @ 440 MHz with 512 MB RAM, picked up from a previous workplace. Great machine for Solaris 10.
- **Apple PowerBook G4** — G4 @ 1.67 GHz, maxed out at 2 GB RAM. Beautiful machine for the time, great screen and ran OS X up until Snow Leopard.
- **Apple Power Mac G4 Cube** — Picked up cheaply on eBay after wanting one since release but not being able to afford it. Didn't get much use out of it, so I passed it on.
