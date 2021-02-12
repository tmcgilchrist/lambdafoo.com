---
title: Streaming iTunes from Solaris 10
author: Tim McGilchrist
date: 2008-12-29 00:00
tags: solaris itunes
description: Streaming iTunes from Solaris 10
---
**Problem:** Sharing iTunes library from home server running Solaris.

**Solution:** Firefly Media Server (aka mt-daapd).

Firefly acts as a shared iTunes volume to any iTunes clients. Since all my
clients will be Macs this is the perfect solution. I had previously tried to
follow
[these](http://blogs.sun.com/constantin/entry/solaris_home_part_4_streaming)
instructions but had no luck.

Coming back with a fresh Solaris 10 and Sun Studio 11 install I had a bit more luck.

**Step 1:** Dependencies available from Blastwave.

 * libid3tag
 * sqlite3

**Step 2:** Download Firefly from [here](http://www.fireflymediaserver.org/). I
  used the latest nightly svn download.

**Step 3:** Running the configure script.

A couple of pre-requisites before running configure, make sure you have the Sun
Studio compilers in your PATH. Otherwise it will try to use the gcc compiler or
not find one at all.

``` shell
export PATH=/opt/SUNWspro/bin:$PATH

./configure --enable-sqlite3 --with-sqlite3-includes=/opt/csw/include \
 -with-sqlite3-libs=/opt/csw/lib \
--with-gdbm-includes=/opt/csw/include --with-gdbm-libs=/opt/csw/lib \
 LDFLAGS="-L/opt/csw/lib" \
CPPFLAGS="-I/opt/csw/include" --prefix=/opt/local

```

Which looks for the sqlite3 and gdbm libraries from my Blastwave install, and
will install the final binary under **/opt/local**. Change this if you want
to install elsewhere.

**Step 4:** Compiling
Run a quick make to compile everything,

``` shell

gmake
gmake install

```

### NOTE Fri Feb  3 08:25:48 ###
I no longer have a Solaris install available, or a SPARC machine for that
matter, so I can't check whether these instructions still work.
