---
layout: post
title: "Thoughts on GNU Hurd"
date: 2012-02-01 10:34
comments: true
categories:
 - Hurd
 - Musings
---

After reading a recent
[article](http://news.ycombinator.com/item?id=1474941)
about GNU Hurd on hacker news I'm struck by the thought that GNUs development of Hurd
is a perfect example of a failed software project.

There was an initial great idea to build a
[microkernel](http://en.wikipedia.org/wiki/Microkernel) based OS with all these
cool features. But what crippled the project was a combination of:

 * constantly switching priorities and technology. How many different kernels
   did they consider and start work on only to abandon work when something
   better/shinier came along.
 * failure to focus on getting something working and out the door. Great teams ship software!
 * they didn't capture a community around developing Hurd like Linux and BSD have.
 * seems like they followed the Big Design up front approach to software
   development whilst Linux went for the more agile iterative approach. Thus
   capturing all the positive feedback and momentum that comes with getting
   customer input and alway having a working product.

Despite all this the idea of a Hurd system is still attractive to many and
personally I look upon it with some nostalgia. I was completely obsessed by
operating systems and coding them around 2000.

I'd love to see a successful Hurd project running on something like L4. But
would such a system gain enough interest in the current open source
environment. If Hurd was available before Linux it seems like it would have
taken off and they could have rode the wave of open source enthusiasm. Now it
seems to me that the space is pretty full and a new os has little opportunity to
gain real market share. Surely Linux and *BSDs are good enough for people interested
in a Hurd system.

Anyway this is all irrelevant until they have something working that people can
run, even in a limited sense. Debian has a
[port](http://www.debian.org/ports/hurd/) but it seems progress is slow.

**Why people dont consider gnu projects?**
Poor public image, it doesn't appear fun to work on gnu. Look at the buzz around
rails for how to manage your public image. Restrictive coding standards and
legal hassle.

[Original link](http://www.h-online.com/open/features/GNU-HURD-Altered-visions-and-lost-promise-1030942.html)
