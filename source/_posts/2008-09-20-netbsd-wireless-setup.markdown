---
layout: post
title: "NetBSD wireless setup"
categories:
- NetBSD
---

After installing NetBSD 4, the ethernet connection worked but not the
wireless. The easiest option available was a 3Com CRWE777A wireless PCMCIA card,
which originally came with a PCI bridge but I'm using it as a normal PCMCIA
card.


Searching around the man pages for various BSD variants showed that the wi(4)
driver should work with this card. Unfortunately the NetBSD source doesn't have
a Product Id for this particular card under the PCMCIA, but it does for the
PCI. Strange!

So it was a simple case of putting the Product Id into 'pcmciadevs':

    product 3COM 3CRWE777A          0x0777 3Com AirConnect CRWE777A Wireless LAN

Then run:

    make -f Makefile.pcmciadevs

which generates the correct structs in the header files.
</p>
<p>
A short kernel recompile later, making sure wi is enabled for pcmcia, and the card should be recognised on startup.
</p>
<p>
Here is the <b>dmesg</b> output from my laptop.
</p>
<pre>
wi0 at pcmcia0 function 0: <3Com, 3CRWE777A AirConnect Wireless LAN PCI Card, Version 01.02, >
wi0: 802.11 address 00:01:03:78:e6:7f
wi0: using RF:PRISM2 MAC:HFA3841 CARD:HWB3163 rev.A
wi0: Intersil Firmware: Primary (0.3.0), Station (0.7.6)
wi0: 11b rates: 1Mbps 2Mbps 5.5Mbps 11Mbps
</pre>
<p>
If not something like this might help:
</p>
<pre>
ifconfig wi0 create
</pre>
<p>
On an unsecured 802.11b wireless network this should work to associate it with <b>test-network</b> and get you an IP. Modify the network name and IP range for you local conditions.
</p>
<pre>

ifconfig wi0 ssid test-network
ifconfig wi0 192.168.0.5 netmask 255.255.255.0

</pre>
<h4> Note:</h4>
<p>
I'm not sure whether this works with WPA correctly. I personally haven't had it working but I'll update this entry if I do. <br/>

The original instructions on how to do this came from <a href="http://www.sfc.wide.ad.jp/~hiddy/software/wi_pci_netbsd/index.html">here </a><br/>

How to compile a NetBSD kernel is explained on the <a href="http://www.netbsd.org/docs/guide/en/chap-kernel.html"> NetBSD site.</a><br/>
</p>
