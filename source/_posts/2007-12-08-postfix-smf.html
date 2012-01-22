--- 
layout: post
title: Postfix & SMF
tags: 
- Postfix
- Solaris
status: publish
type: post
published: true
meta: 
  _aioseop_title: Postfix + SMF
  _aioseop_keywords: postfix, solaris, solaris 10, smf, blastwave
  _aioseop_description: How to convert Blastwave postfix to use SMF
---
Blastwave Postfix doesn't currently use SMF, but I really wanted it to. So I converted the SysV init script supplied into a more usable form. I'll assume you've installed Postfix and have it configured the way you like. There are lots of tutorials around that can give you a start on how to do that, personally I've found the Gentoo documentation quite useful even on non-Linux systems.


Type the following into a root shell.


<code>$ svcs |grep postfix</code><br/>
<code>legacy_run     20:09:50 lrc:/etc/rc2_d/S50postfix </code>


this means that SMF is using the old style init scripts.


The first thing we need to do is write a series of targets to let SMF know how to stop, start and reload postfix. Luckily Blastwave comes with such a script, the init script under <strong>/etc/init</strong>. So what I did was copy that to the same place as the other Blastwave scripts live <strong>/opt/csw/lib/svc/method/</strong> and renamed it to<strong> svc-postfix</strong>, following the naming convention used for other services like Apache and MySQL. Or you can download it here <a href="http://gothmog.homeunix.net/downloads/smf-postfix/svc-postfix">Methods</a>.


Also check that the ownership permissions are the same as the other files in there, I had to <strong>chown root:bin svc-postfix</strong> the file so it matched the others.


Now we need a manifest, I used the existing manifest for Blastwave Apache as a guide to writing mine. 
Here is the <a href="http://gothmog.homeunix.net/downloads/smf-postfix/cswpostfix.xml">Manifest</a> which you need to copy this into <strong>/opt/csw/var/svc/manifest/site</strong>. 


Now to import the manifest so SMF knows about it.

<code>$ /usr/sbin/svccfg -v import \ /opt/csw/var/svc/manifest/site/cswpostfix.xml</code>


The new service should now appear in the list of available services.


<code>$ svcs -a |grep postfix</code><br/>
<code>online         Dec_06   svc:/network/smtp:cswpostfix</code>


Now to disable the old init script.

<code>$ cd /etc/rc2.d</code>


Stop the service

<code>$ ./S50postfix stop</code>

Move the symlink out of harms way, I usually don't delete them incase I need them later, like if I disable a core service.


<code>$ mv S50postfix noS50postfix</code>

And ensure that it's no longer executable, probably overkill but we really don't want it running.

<code>$ chmod 000 noS50postfix</code>


Then we can enable the fancy new SMF service.
<code>$ svcadm enable cswpostfix</code>


Check that is started properly.
<code>$  svcs -x</code>


And that should be it, it's usually a good idea to reboot when you've been messing with startup scripts. It ensures that you haven't made any mistakes configuring things and the last thing you want is to find you messed up when something really does break.


Note: This is my first go at converting a SysV init script into an SMF version so if there are any glaring mistakes, please drop me a line.


The inspiration for this post supplied by:<a href="http://prefetch.net/blog/index.php/2005/11/12/converting-an-rc-script-to-an-smf-manifest/">Converting an rcscript into an smf manifest</a>.

Downloads
<a href="http://gothmog.homeunix.net/downloads/smf-postfix/cswpostfix.xml">Manifest</a>
<a href="http://gothmog.homeunix.net/downloads/smf-postfix/svc-postfix">Methods</a>
 
