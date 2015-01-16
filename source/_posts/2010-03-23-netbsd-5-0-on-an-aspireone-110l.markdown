---
layout: post
title: NetBSD 5.0 On An AspireOne 110L
categories:
- NetBSD
---
After trying Ubuntu Linx on the AspireOne for a few months, I wanted to branch
out and try something a bit different. Linux is so main stream theses days, I
only had to mess around with the wireless. Far too easy. Why back in my day we
entered our X11 config on punchcards!!!

Anyway, my options were either OpenBSD or NetBSD.

I've wanted to try OpenBSD on the desktop for ages, but being a bit fresh at the
BSDs I decided to go for NetBSD. It was a bit less daunting and there are more
online resources for NetBSD. But there is still something about OpenBSD, I'm
sure I'll get around to it soon.

So I started off with downloading the ISO images for the latest
[NetBSD](http://www.netbsd.org/releases/formal-5/NetBSD-5.0.html)
release 5.0. The latest release of NetBSD includes improved ACPI support and the
**ath(4)** wireless is (apparently) supported. Plus all the other goodies
mentioned in the release notes.

## NetBSD Install ##

NetBSD has a basic terminal based installer, anyone with some Unix experience
should be comfortable and even without any experience, just reading the
[installer](http://www.netbsd.org/docs/guide/en/index.html) guide should get you
a working system.

My AspireOne 110L came with:

 * Intel Atom 270 (1.6Ghz)
 * 1.5Gb RAM (upgraded from the base 512MB)
 * 8Gb SSD, and
 * 8.9"screen @ 1024x600

No CD drive meant that I needed a bootable USB drive. I followed these
[basic guidelines](http://wiki.netbsd.se/How_to_install_NetBSD_from_an_USB_Memory_Stick). Since
this page was written for 4.0 some of the steps are different.

Step 7. change the url from

    ftp -a ftp://ftp.netbsd.org/pub/NetBSD/iso/4.0.1/i386cd-4.0.1.iso

to

    http://ftp.netbsd.org/pub/NetBSD/iso/5.0/i386cd-5.0.iso

At Step 9 change it to look like this:

    mount /dev/sd0a /stick
    cp /image/i386/binary/kernel/netbsd-FLOPPY.gz /stick/netbsd.gz
    cp -R /image/i386/binary/sets /stick/sets
    umount /stick
    rmdir /stick

I'm not sure I used the correct kernel here, the name suggests a bootable floppy
kernel rather than the normal install kernel. I tried the netbsd-GENERIC.tgz but
it started looking for things like "init" and "/etc/rc" which didn't seem right
so I went back to the other kernel.

After I had the bootable USB setup correctly I rebooted the Aspire and started
the install. Initially I tried following the instructions on installing from the
*sets* copied onto the USB stick but the installer couldn't find them. So In the
end I went for the HTTP install
([Figure 3.20](http://www.netbsd.org/docs/guide/en/chap-exinst.html#inst-medium)
from the Install Guide), a few hours later the installed finished. Rebooting the
system brought this greeting:

    Last login: Mon May 18 22:09:59 2009 from 172.17.0.10
    NetBSD 5.0 (GENERIC) #0: Sat May 16 18:00:22 EST 2009
    Welcome to NetBSD!
    micro$

After poking around for a bit the 2 main outstanding issues are: X11 Setup and
Wireless.

## X11 Setup ##

Here the NetBSD docs are really lacking, they still explain how to configure
XFree86 which is a complete pain. Instead NetBSD is shipping with Xorg,
providing a vastly superior configuration experience.

Simply run **X -configure** as root. Nice!

It'll dump an **xorg.conf.new** in root's home directory.

To test the server with this file, run **X -config /root/xorg.conf.new**.

And if you're happy with it copy it over to **/etc/X11/xorg.conf**.

## Wireless ##

The **ath(4)** driver doesn't attach correctly with the default GENERIC kernel,
so some customisation is required. Apply this patch:

    ftp://ftp.netbsd.org/pub/NetBSD/misc/jmcneill/old/atheee.patch

Recompile the GENERIC kernel and reboot.

All the steps are documented here:

[http://www.netbsd.org/docs/guide/en/chap-tuning.html#tuning-kernel-prepare](http://www.netbsd.org/docs/guide/en/chap-tuning.html#tuning-kernel-prepare)

After a reboot it's recognised:

     ath0 at pci3 dev 0 function 0
     ath0: interrupting at ioapic0 pin 18
     ath0: 11b rates: 1Mbps 2Mbps 5.5Mbps 11Mbps
     ath0: 11g rates: 1Mbps 2Mbps 5.5Mbps 11Mbps 6Mbps 9Mbps 12Mbps 18Mbps 24Mbps 36Mbps 48Mbps 54Mbps

## Things to fix up ##

Installing NetBSD and getting productive certainly isn't the seamless process
that Ubuntu was. So there are a few minor things that need a bit of fixing.

 * Wireless on WPA2: Linux worked on this so I'm sure NetBSD can.
 * Booting straight into X: It's in the docs so it won't be hard.
 * Installing a decent WindowManager: XMonad here we come :-)

### Resources: ###
 * [http://wiki.netbsd.se/Acer_Aspire_one](http://wiki.netbsd.se/Acer_Aspire_one)
 * [http://www.netbsd.org/docs/guide/en/](http://www.netbsd.org/docs/guide/en/)
 * [http://mail-index.netbsd.org/current-users/2008/08/21/msg004079.html](http://mail-index.netbsd.org/current-users/2008/08/21/msg004079.html)
