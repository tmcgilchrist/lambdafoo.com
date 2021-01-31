---
title: "Quick OS X Tip: crontab entry to startup a GUI application on OS X"
author: Tim McGilchrist
date: 2008-10-06
tags: OSX
description: "Quick OS X Tip: crontab entry to startup a GUI application on OS X"
---

Just a quick note, about crontab and OS X.

If you want to startup a Cocoa GUI application from the commandline you can use
[open](http://developer.apple.com/documentation/Darwin/Reference/ManPages/man1/open.1.html).

What I've done to startup my bittorrent client during off-peak hours is add this to my crontab:

    15      2       *       *       *       /usr/bin/open /Applications/Vuze.app >& /dev/null

Which will startup Vuze at 2:15 am, the /dev/null part helps to suppress any warnings or errors.

### Background ###

Some background on the issue, I have a ADSL2+ connection that is split between
15Gb peak and 20Gb off-peak, where the offpeak-hours are between 2 am and 12
am. So this crontab is perfect for using up all those off-peak Mbs that I would
otherwise waste.

### TODO ###
The only remaining issue is that I have no good way of killing the application
when it gets to 12:00. For that I'd ideally use **pkill** but there isn't a
default or good implementation of that for OS X. The best remaining option is to
write a ruby/python script to kill the app instead.

### Update ###
The following entry in your crontab **should** kill off Vuze at 11.

    *	11	*	*	*	kill `ps -A | cut -c 1-6,25-120 |grep Vuze.app |cut -c 1-6` >& /dev/null

Nasty sure, but it seems to be effective. Please test liberally on your own
machine to make sure it behaves as required.
