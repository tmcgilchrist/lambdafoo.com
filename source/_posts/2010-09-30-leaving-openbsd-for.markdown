---
layout: post
title: Leaving OpenBSD for
tags:
- Linux
- OpenBSD
status: publish
type: post
published: true
---
I'm migrating my home server away from OpenBSD.

The things I was able to setup in OpenBSD have worked very well. Things like
DHCP, DNS and SQUID have worked excellently. I found some fantastic guides on
http://www.kernel-panic.it/openbsd.html that allowed me to
get everything up and working in no time at all. I wont bother posting any
details from my setup it's better to just visit that site and decide which
pieces you want.

Why move away? These are surely very personal reasons to make the switch and are
not a general condemnation of OpenBSD which is still a fantastic piece of
software.

 1. I wasn't able to get DHCP &amp; DNS to dynamically update DNS when a new host
   was added to the network. Instead I had a whole heap of static mappings for
   computers. Not an ideal situation!

 2. The upgrade path for OpenBSD confuses me. When a new version comes out how do
	I upgrade without nuking all my local configuration? I really don't want to
	go back through the setup every 6 months when the BSD guys bring out a new
	version.

 3. I wasn't able to get PHP + MySQL + Wordpress to play nicely together. Not
	being a PHP person I didn't really grok how to setup the whole thing so I
	was forced to follow guides written by others. OpenBSD doesn't really seem
	to be a Wordpress/PHP platform of choice for many so I struggled to find
	accurate docs on what to do.

 4. I wanted to add new services to the server like Ruby/Rails, Java/EE,
 Python/Django and Erlang/OTP. Doing all this and keeping it current on OpenBSD
 is more work that I was willing to expend.

So the solution I'm turning to is Linux and specifically Ubuntu in all it's glory.

What a spiffy system Ubuntu is? I remember struggling to install Debian way back
when but Ubuntu is a pleasure and perhaps a little too easy. Kids these days
don't need to struggle for hours setting up XFree86 or a dodgy network card.

So over the next few weeks I'm migrating the essential services, DHCP, DNS and
SQUID to Ubuntu. After that I'll get a bit funky and start adding this blog on
there and other bits and pieces.

Hooray for Ubuntu!!
