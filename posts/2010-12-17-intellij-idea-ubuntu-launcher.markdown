---
title: IntelliJ IDEA Ubuntu Launcher
author: Tim McGilchrist
date: 2010-12-27 00:00
tags: Java
description: IntelliJ IDEA Ubuntu Launcher
---

I've been using IntelliJ on Ubuntu 10.10 lately for work and rather than
starting things from the terminal I wanted a pretty launcher in the top menu.

After a quick search I found this site:

[IntelliJ Ubuntu Launcher](http://www.dotkam.com/2010/06/16/intellij-idea-ubuntu-launcher/)

Following the instructions yielded a pretty launcher to sit in my top menu bar.

The only improvement I could add is to have an **/opt/intellij-current** directory and use

    sudo ln -s /opt/idea-IU-99.18 /opt/idea-current

to redirect it around to point to the current version of IntelliJ you have.
