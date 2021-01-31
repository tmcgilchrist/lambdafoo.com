---
layout: post
title: Installing NetBSD on a Dell Inspiron 8000
tags:
- NetBSD
---

I just finished installing NetBSD on my old laptop and I must say it's quite an
impressive system especially when it comes to updating to a new release.

The victim is my ancient Dell Insprion 8000, featuring a 800Mhz P3 and 384Mb
RAM. Not exactly a powerhouse but the 15" 1400x1050 screen makes up for the lack
of power.

To understand the various steps I had to take, it's important to note that the
DVD drive is flakey and it has no native Network adapter. Stupidly I tried to
save money and left it out at the time.

Now to the details.

I originally wanted [OpenBSD](http://www.openbsd.org) on this thing
but none of the floppy boot disks supported the PCMCIA network cards I have, and
I'm too new to BSDs to know how to fix this. Also none of the recent
[NetBSD](http://www.netbsd.org) boot CDs would work, they kept getting IO errors
from the DVD drive.

Finally ended up getting an ancient NetBSD 2.0 cd to work and did an install of
that. Somehow the 2.0 installer didn't hit any of the IO errors that stopped the
other installs. The PCMCIA network worked and after a bit of messing about with
the NetBSD Guide I was up and running on the home network.

Now for the impressive part upgrading to 4.0.

* Make a backup. Not required in this case.
* Fetch a new kernel and the binary sets and store them /some/where/
* Install the new kernel (but keep the old one!!)

``` shell

    mv /netbsd /netbsd.old
    pax -zrf /some/where/newkernel.tgz
    mv /some/where/newkernel /netbsd

```

* Reboot using **reboot**
* Install all the sets *except* etc.tzg and xetc.tgz!!

``` shell

    cd /
    pax -zrf /some/where/set.tgz
    ...

```

* Run etcupdate to merge important changes:

``` shell

    cd /
    etcupdate -s /some/where/etc.tgz -s /some/where/xetc.tgz

```

* Upgrade finished, time to reboot.

These instructions are originally from
[the NetBSD mailing list](http://mail-index.netbsd.org/netbsd-help/2008/03/04/msg000089.html). Too
easy! The etcupdate set takes a while and I must admit I did stuff it up (a few
times), but this is much easier that I remember from my last Linux update. Now
to recompile the kernel and get the 3Com AirConnect 3CRWE777A card working.
