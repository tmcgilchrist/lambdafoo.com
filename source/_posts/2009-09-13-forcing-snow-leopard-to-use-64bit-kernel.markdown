---
layout: post
title: Forcing Snow Leopard to use 64bit Kernel
tags:
- OS X
status: publish
type: post
published: true
---
How to force Snow Leopard to boot the 64 bit kernel, cause we all want to run the
latest 64 bit goodness from Apple.

Open the file

    /Library/Preferences/SystemConfiguration/com.apple.Boot.plist

with something like TextEdit and add  arch=x86_64 to the Kernel Flags string:

{% codeblock lang:xml %}

<key>Kernel</key>
<string>mach_kernel</string>
<key>Kernel Flags</key>
<string>arch=x86_64</string>

{% endcodeblock %}

This is posted just about everywhere, and is really only added here for my convenience.
