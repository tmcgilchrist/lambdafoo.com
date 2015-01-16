---
layout: post
title: Postfix & SMF
categories:
  - Postfix
  - Solaris
---
Blastwave's Postfix doesn't currently use SMF, but I really wanted it to. So I
converted the SysV init script supplied into a more usable form. I'll assume
you've installed Postfix and have it configured the way you like. There are
lots of tutorials around that can give you a start on how to do that,
personally I've found the Gentoo documentation quite useful even on non-Linux
systems.


Type the svcs command into a root shell like so:

```
$ svcs |grep postfix
legacy_run     20:09:50 lrc:/etc/rc2_d/S50postfix
```

this means that SMF is using the old style init scripts.


The first thing we need to do is write a series of targets to let SMF know how
to stop, start and reload postfix. Luckily Blastwave comes with such a script,
the init script under **/etc/init**. So what I did was copy that to
the same place as the other Blastwave scripts live
**/opt/csw/lib/svc/method/** and renamed it to
**svc-postfix**, following the naming convention used for other services
like Apache and MySQL.

Also check that the ownership permissions are the same as the other files in
there, I had to **chown root:bin svc-postfix** the file so it matched the others.


Now we need a manifest, I used the existing manifest for Blastwave Apache as a
guide to writing mine. Here is the
[Manifest](http://gothmog.homeunix.net/downloads/smf-postfix/cswpostfix.xml)
which you need to copy this into **/opt/csw/var/svc/manifest/site**.

Now to import the manifest so SMF knows about it.

```
$ /usr/sbin/svccfg -v import /opt/csw/var/svc/manifest/site/cswpostfix.xml
```

The new service should now appear in the list of available services.

```
$ svcs -a |grep postfix
online         Dec_06   svc:/network/smtp:cswpostfix<
```

Now to disable the old init script.

```
$ cd /etc/rc2.d
```

Stop the service

```
$ ./S50postfix stop
```

Move the symlink out of harms way, I usually don't delete them incase I need
them later, like if I disable a core service.

```
$ mv S50postfix noS50postfix
```

And ensure that it's no longer executable, probably overkill but we really don't
want it running.

```
$ chmod 000 noS50postfix
```

Then we can enable the fancy new SMF service.

```
$ svcadm enable cswpostfix
```

Check that is started properly.

```
$  svcs -x
```

And that should be it, it's usually a good idea to reboot when you've been
messing with startup scripts. It ensures that you haven't made any mistakes
configuring things and the last thing you want is to find you messed up when
something really does break.

#### Note: ####
This is my first go at converting a SysV init script into an SMF version
so if there are any glaring mistakes, please drop me a line.

The inspiration for this post supplied by:
[Converting an rcscript into an smf manifest](http://prefetch.net/blog/index.php/2005/11/12/converting-an-rc-script-to-an-smf-manifest/)

#### Downloads ####
Currently offline due to blog conversion to markdown, will post in github soon.

<a href="http://gothmog.homeunix.net/downloads/smf-postfix/cswpostfix.xml">Manifest</a>
<a href="http://gothmog.homeunix.net/downloads/smf-postfix/svc-postfix">Methods</a>
