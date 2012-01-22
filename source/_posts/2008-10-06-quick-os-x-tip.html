--- 
layout: post
title: Quick OS X Tip
tags: 
- OS X
status: publish
type: post
published: true
meta: 
  _aioseop_title: crontab entry to startup a GUI application on OS X
  _edit_last: "1"
---
<p>
Just a quick note, about crontab and OS X.
</p>
<p>
If you want to startup a Cocoa GUI application from the commandline you can use <a href="http://developer.apple.com/documentation/Darwin/Reference/ManPages/man1/open.1.html">open</a>.
</p>
<p>
What I've done to startup my bittorrent client during off-peak hours is add this to my crontab:
<pre>
15      2       *       *       *       /usr/bin/open /Applications/Vuze.app >& /dev/null
</pre>
Which will startup Vuze at 2:15 am, the /dev/null part helps to suppress any warnings or errors.
</p>
<p>
<h4> Background </h4>
Some background on the issue, I have a ADSL2+ connection that is split between 15Gb peak and 20Gb off-peak, where the offpeak-hours are between 2 am and 12 am. So this crontab is perfect for using up all those off-peak Mbs that I would otherwise waste.
</p>
<p>
<h4> TODO </h4>
The only remaining issue is that I have no good way of killing the application when it gets to 12:00. For that I'd ideally use <i>pkill</i> but there isn't a default or good implementation of that for OS X. The best remaining option is to write a ruby/python script to kill the app instead.
</p>
<p>
<h4>Update:</h4>
The following entry in your crontab <i> should </i> kill off Vuze at 11.
<pre>
*	11	*	*	*	kill `ps -A | cut -c 1-6,25-120 |grep Vuze.app |cut -c 1-6` >& /dev/null
</pre>
Nasty sure, but it seems to be effective.
</p>
