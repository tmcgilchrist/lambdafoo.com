---
title: Creating an rsync copy of Nekoware
author: Tim McGilchrist
date: 2008-01-06 00:00
tags:
- IRIX
- SGI
description: Creating an rsync copy of Nekoware
---

If you're running an SGI at home or maybe at work, and you haven't heard of
-Nekoware, then you must be new around here. It's a collection of opensource
-software all compiled and ready to install for your SGI. Check out
-[Nekoware](http://www.nekochan.net/downloads.php) for a complete list of what
-is available .

What I want to do here is show how you can have your very own copy of Nekoware locally.

Download a copy of Nekoware rsync from one of the mirrors, I used this
[Mirror](http://www.mechanics.citg.tudelft.nl/~everdij/nekoware/index.php?path=current/)
and downloaded the file in a browser.

Installing rsync really depends on whether you have a monitor hooked up to your SGI or you're using ssh/remote X , note that telnet is **not** an option. I'm using remote X so that's what is shown here.

Login as root and change to the directory where you downloaded rsync to.

    cd /usr/people/me/downloads

To install software from the command line you need to use the software
installation tool **inst(1)**.

``` shell

inst

Default distribution to install from: /var/tmp/tardista000i0

For help on inst commands, type "help overview".


Inst 4.1 Main Menu

 1. from [source ...]            Specify location of software to be installed
 2. open [source ...]            Specify additional software locations
 3. close [source ...]           Close a software distribution location
 4. list [keywords] [names]      Display information about software subsystems
 5. go                           Perform software installation and removal now
 6. install [keywords] [names]   Select subsystems to be installed
 7. remove [keywords] [names]    Select subsystems to be removed
 8. keep [keywords] [names]      Do not install or remove these subsystems
 9. step [keywords] [names]      Interactive mode for install/remove/keep
10. conflicts [choice ...]       List or resolve installation conflicts
11. help [topic]                 Get help in general or on a specific word
12. view ...                     Go to the View Commands Menu
13. admin ...                    Go to the Administrative Commands Menu
14. quit                         Terminate software installation

Inst>

# from /usr/people/me/downloads/neko_rsync-2.6.8.tardist

Unpacking tardist file into temporary distribution directory /var/tmp/tardista0
00F7
Unpacking tardist file .. 100% Done.
Reading product descriptions ..  13%
Reading /var/inst/hist
Reading product descriptions ..  25%
Setting distribution to /var/tmp/tardista000F7
Reading product descriptions .. 100% Done.

```

List the files available in this package

``` shell

Inst> list
  View:      distribution
  Status:    N=new, U=upgrade, S=same, D=downgrade
  Stream:    feature
  Selection: i=install, r=remove, k=keep

  Subsystem Types [bdro]:  b=reBoot needed, d=Default, r=Required, o=overlay

  S  neko_rsync.man.manpages [d]        0   man pages
  S  neko_rsync.opt.dist                0   distribution files
  S  neko_rsync.opt.relnotes            0   release notes
  S  neko_rsync.opt.src                 0   original source code
  S  neko_rsync.sw.eoe [d]              0   execution only env

Disk space summary (Kbytes):            /

Current free space                 981640
- Selections net change                 0
- Temporary inst overhead               0
= Minimum free during install      981640

Final projected free space         981640

```

Now select the files you want installed, I went for the default which includes the executable and the man page.

``` shell

Inst> install d

Inst> list
  View:      distribution
  Status:    N=new, U=upgrade, S=same, D=downgrade
  Stream:    feature
  Selection: i=install, r=remove, k=keep

  Subsystem Types [bdro]:  b=reBoot needed, d=Default, r=Required, o=overlay

i S  neko_rsync.man.manpages [d]        0   man pages
  S  neko_rsync.opt.dist                0   distribution files
  S  neko_rsync.opt.relnotes            0   release notes
  S  neko_rsync.opt.src                 0   original source code
i S  neko_rsync.sw.eoe [d]              0   execution only env

Disk space summary (Kbytes):            /

Current free space                 981640
- Selections net change                 0
- Temporary inst overhead            1628+
= Minimum free during install      980012

Final projected free space         981640

Inst>

```

All that remains is the install, so type **go** and watch it go.  After it has
finished installing, you can simply quit isnt.

```

You may continue with installations or quit now.

Inst> quit
The tardist distribution neko_rsync-2.6.8.tardist has been unpacked as /var/tmp/
tardista000Fq.
Do you want to save this distribution for future installations,
or remove it?
1. Save this distribution
2. Remove this distribution
Please enter a choice [2]: 2
Requickstarting ELF files (see rqsall(1)) ..  25%

```

The requickstarting stage may take a while, so just leave it to do it's thing.

Once finished it's time to rsync some Nekoware. Open up a terminal, it doesn't have to be as root, create a directory and change into it.

    mkdir nekoware
    cd nekoware

I only wanted a copy of the current MIPS4 packages, you will most likely want
the same, unless you have a really old SGI. So copy the following command and
wait for it to finish, which may be a while for a new copy.

    /usr/nekoware/bin/rsync -az --progress \
    mech001.citg.tudelft.nl::nekoware/current  /usr/people/me/nekoware

Once finished we have an rsync copy of nekoware, that we can install from and
can be upgraded when new versions of packages come out. To upgrade simply run
the previous command again, and it should only download the updated/changed
packages.

For futher information about rsync and all the brilliant things it can do visit
[rsync homepage](http://samba.anu.edu.au/rsync/)
