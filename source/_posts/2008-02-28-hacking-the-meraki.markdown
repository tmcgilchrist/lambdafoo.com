--- 
layout: post
title: Hacking the Meraki
tags: 
- Coding
- Meraki
status: publish
type: post
published: true
meta: 
  _edit_last: "1"
---
<p>Success!</p>
<p>The Meraki Mini has now had it's internals revealed and a serial cable attached.</p>
<p>It's ready for real work now!</p>

<pre>
Ethernet eth0: MAC address 00:18:XX:XX:XX:XX
IP: 192.168.XX.XX/255.255.255.0, Gateway: 0.0.0.0
Default server: 192.168.XX.XX

RedBoot(tm) bootstrap and debug environment [ROMRAM]
Release, version V1.04 - built 12:24:00, Apr 17 2006

Copyright (C) 2000, 2001, 2002, 2003, 2004 Red Hat, Inc.

Board: Meraki Mini
RAM: 0x80000000-0x82000000, [0x8003d110-0x80fe1000] available
FLASH: 0xa8000000 - 0xa87e0000, 128 blocks of 0x00010000 bytes each.
== Executing boot script in 2.000 seconds - enter ^C to abort
^C
RedBoot>
</pre>
<p>Continue....[Y/n]</p>
<pre>
RedBoot> load art_mini.elf
Using default protocol (TFTP)
__udp_sendto: Can't find address of server
Can't load 'art_mini.elf': some sort of network error
RedBoot> go
No entry point known - aborted
RedBoot> load -h 192.168.XX.X -p 80 -m http /meraki/mini.1.img
Unable to reach host 192.168.XX.X (192.168.XX.X)
RedBoot> exec
Can't execute Linux - invalid entry address
RedBoot> fis load stage2
RedBoot> exec
Now booting linux kernel:
 Base address 0x80030000 Entry 0x80100000
 Cmdline :
starting stage2
reading flash at 0xa8150000 - 0xa8413a90... done
Calculating CRC... 0xcf267f1d - matches
decompressing... done
starting linux
Linux version 2.6.16.16-meraki-mini (cliff@bug.meraki.net) (gcc version 3.4.6 (OpenWrt-2.0)) #2187
CPU revision is: 00019064
Determined physical RAM map:
 memory: 02000000 @ 00000000 (usable)
Built 1 zonelists
Kernel command line:
Primary instruction cache 16kB, physically tagged, 4-way, linesize 16 bytes.
Primary data cache 16kB, 4-way, linesize 16 bytes.
Synthesized TLB refill handler (20 instructions).
Synthesized TLB load handler fastpath (32 instructions).
Synthesized TLB store handler fastpath (32 instructions).
Synthesized TLB modify handler fastpath (31 instructions).
PID hash table entries: 256 (order: 8, 4096 bytes)
Using 92.000 MHz high precision timer.
Dentry cache hash table entries: 8192 (order: 3, 32768 bytes)
Inode-cache hash table entries: 4096 (order: 2, 16384 bytes)
Memory: 20664k/32768k available (2154k kernel code, 12088k reserved, 398k data, 8832k init, 0k hi)
Mount-cache hash table entries: 512
Checking for 'wait' instruction...  available.
unpacking initramfs....done
NET: Registered protocol family 16
Algorithmics/MIPS FPU Emulator v1.5
JFFS2 version 2.2. (NAND) (C) 2001-2003 Red Hat, Inc.
Initializing Cryptographic API
io scheduler noop registered
io scheduler anticipatory registered (default)
io scheduler deadline registered
io scheduler cfq registered
faulty_init
watchdog hb: 90  ISR: 0x21  IMR: 0x8  WD : 0x8eac3ead  WDC: 0x0
ar2315_wdt_init using heartbeat 90 s cycles 3600000000
watchdog hb: 90  ISR: 0x21  IMR: 0x88  WD : 0xd68e7abf  WDC: 0x0
Serial: 8250/16550 driver $Revision: 1.90 $ 1 ports, IRQ sharing disabled
serial8250: ttyS0 at MMIO 0xb1100003 (irq = 37) is a 16550A
eth0: Dropping NETIF_F_SG since no checksum feature.
eth0: Atheros AR2313: 00:18:0a:01:56:72, irq 4
tun: Universal TUN/TAP device driver, 1.6
tun: (C) 1999-2004 Max Krasnyansky <maxk@qualcomm.com>
MTD driver for SPI flash.
spiflash: Probing for Serial flash ...
spiflash: Found SPI serial Flash.
8388608: size
RedBoot partition parsing not available
Creating 7 MTD partitions on "spiflash":
0x00000000-0x00030000 : "RedBoot"
0x00030000-0x00050000 : "stage2"
0x00050000-0x00150000 : "/storage"
0x00150000-0x00490000 : "part1"
0x00490000-0x007d0000 : "part2"
0x007d0000-0x007e0000 : "redboot config"
0x007e0000-0x00800000 : "board config"
oprofile: using timer interrupt.
NET: Registered protocol family 2
IP route cache hash table entries: 512 (order: -1, 2048 bytes)
TCP established hash table entries: 2048 (order: 2, 16384 bytes)
TCP bind hash table entries: 2048 (order: 2, 16384 bytes)
TCP: Hash tables configured (established 2048 bind 2048)
TCP reno registered
ip_conntrack version 2.4 (256 buckets, 2048 max) - 232 bytes per conntrack
ip_conntrack_pptp version 3.1 loaded
ip_nat_pptp version 3.0 loaded
ip_tables: (C) 2000-2006 Netfilter Core Team
ClusterIP Version 0.8 loaded successfully
TCP bic registered
NET: Registered protocol family 1
NET: Registered protocol family 17
Bridge firewalling registered
Freeing unused kernel memory: 8832k freed
init started:  BusyBox v1.1.0 (2007.03.05-17:58+0000) multi-call binary

Please press Enter to activate this console.

</pre>
<p>
Disassembly instructions and serial port layout taken from <a href="http://wiki.openwrt.org/OpenWrtDocs/Hardware/Meraki/Mini">OpenWRT Meraki</a>.<br />
Hardware sourced from <a href="http://meraki.com/">Meraki</a><br />
USB to Serial, part number FTDI TTL-232R-3V3, cable provided by <a href="http://www.glyn.co.nz/index.htm">GLYN Ltd</a>
</p>
<p>
Pictures to follow.
</p>
