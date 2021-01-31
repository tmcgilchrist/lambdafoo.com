---
title: XMonad Install on NetBSD 5.0
author: Tim McGilchrist
date: 2009-05-30 00:00
tags:
- Haskell
- NetBSD
- xmonad
description: XMonad Install on NetBSD 5.0
---
I had some issues with getting XMonad and it's dependencies working on NetBSD
5.0 with pkgsrc, so I thought I'd put this up to help out others and myself when
I forget how it was done.

Start by installing ghc-6.8.3 from pkgsrc. The two options here are:
 * pkg_add -v ghc
 * cd /usr/pkgsrc/lang/ghc; make; make install

Both require root access. The first option will download a prebuilt binary of
ghc and install it all for you. I chose the second option which involves
downloading the
[pkgsrc source](http://www.netbsd.org/docs/pkgsrc/getting.html#getting-first) and [compiling
from source](http://www.netbsd.org/docs/pkgsrc/using.html#how-to-build-and-install).

Next up you need to install Xmonad's dependencies:
 * [mtl](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/mtl)
 * [unix](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/unix)
 * [X11](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/X11)

Running **ghc-pkg list** will show you what is already installed.

Mine looked something like this:

``` shell

micro# ghc-pkg list
/usr/pkg/lib/ghc-6.8.3/package.conf:
    Cabal-1.2.4.0, HUnit-1.2.0.0, QuickCheck-1.1.0.0, X11-1.4.5,
    array-0.1.0.0, base-3.0.2.0, bytestring-0.9.0.1.1, cgi-3001.1.6.0,
    containers-0.1.0.2, directory-1.0.0.1, fgl-5.4.2.0,
    filepath-1.1.0.0, (ghc-6.8.3), haskell-src-1.0.1.2,
    haskell98-1.0.1.0, hpc-0.5.0.1, html-1.0.1.1, mtl-1.1.0.1,
    network-2.2.0.0, old-locale-1.0.0.0, old-time-1.0.0.0,
    packedstring-0.1.0.0, parallel-1.0.0.1, parsec-2.1.0.1,
    pretty-1.0.0.0, process-1.0.0.1, random-1.0.0.0, readline-1.0.1.0,
    regex-base-0.72.0.1, regex-compat-0.71.0.1, regex-posix-0.72.0.2,
    rts-1.0, stm-2.1.1.1, template-haskell-2.2.0.0, time-1.1.2.1,
    unix-2.3.0.1, xhtml-3000.2.0.0
```

So I only needed X11 which I grabbed from
[here](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/X11). Untar
the X11 download, hop into that directory and run each of these commands in order.

    runhaskell Setup configure
	runhaskell Setup build
	runhaskell Setup install

If it fails, **good** it's just like my install. I got the following stack trace:

``` shell

Building X11-1.4.5...
[22 of 24] Compiling Graphics.X11.Xlib.Extras ( dist/build/Graphics/X11/Xlib/Extras.hs, dist/build/Graphics/X11/Xlib/Extras.o )
Warning: orphan instances:
  instance GHC.Read.Read [Graphics.X11.Xlib.Types.Rectangle]
    = Graphics.X11.Xlib.Extras.$f1

In file included from /usr/pkg/lib/ghc-6.8.3/include/Stg.h:183,
                 from /usr/pkg/lib/ghc-6.8.3/include/Rts.h:19,

                 from dist/build/Graphics/X11/Xlib/Extras_stub.c:2:0:


/usr/pkg/lib/ghc-6.8.3/include/Regs.h:28:17:
     error: gmp.h: No such file or directory
In file included from /usr/pkg/lib/ghc-6.8.3/include/Stg.h:183,
                 from /usr/pkg/lib/ghc-6.8.3/include/Rts.h:19,

                 from dist/build/Graphics/X11/Xlib/Extras_stub.c:2:0:


/usr/pkg/lib/ghc-6.8.3/include/Regs.h:121:0:
     error: expected specifier-qualifier-list before 'MP_INT'

In file included from dist/build/Graphics/X11/Xlib/Extras_stub.c:2:0:


/usr/pkg/lib/ghc-6.8.3/include/Rts.h:203:0:
     error: expected ')' before '*' token

/usr/pkg/lib/ghc-6.8.3/include/Rts.h:204:0:
     error: expected ')' before '*' token

```

Seems that it's actually complaining that it can't see gmp.h.  So the quick 'n nasty fix for this is.

    ln -s /usr/pkg/include/gmp.h /usr/pkg/lib/ghc-6.8.3/include/gmp.h

Idea originally from [here](http://ghcsparc.blogspot.com/2009/01/in-in-morning.html).

Now retry, **runhaskell Setup build** and **runhaskell Setup install**, which
should work now.

Next download the xmonad sources from [http://xmonad.org](http://xmonad.org), I
grabbed both xmonad and xmonad-contrib. Untar them and follow the same
runhaskell commands from before:

    runhaskell Setup configure
    runhaskell Setup build
    runhaskell Setup install

It should now all compile & install correctly, and **you** can get on with using
Xmonad.

Finally here are some helpful Xmonad starter links:

 * [http://xmonad.org/tour.html](http://xmonad.org/tour.html)
 * [http://haskell.org/haskellwiki/Xmonad/Config_archive/John_Goerzen's_Configuration](http://haskell.org/haskellwiki/Xmonad/Config_archive/John_Goerzen's_Configuration)
 * [http://www.xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Doc-Configuring.html](http://www.xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Doc-Configuring.html)
