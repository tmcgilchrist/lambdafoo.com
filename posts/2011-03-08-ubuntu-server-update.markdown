---
title: Ubuntu Server update
author: Tim McGilchrist
date: 2011-03-08 00:00
tags: Unix Linux
description: Ubuntu Server update
---

I posted a while ago about switching to Ubtuntu for my home server.

I've got the basics up an running on my MacMini (artemis). Namely DHCP for the
home network, DNS primarily as a caching DNS server and WWW for hosting my
wordpress steup.

The setup of DHCP was easy enough. Followed the find documentation on the Ubuntu
site. Same story for a caching DNS.

Setup Apache/PHP/MySQL was a quick 'sudo apt-get install php5-mysql' and I
installed Wordpress via subversion checkout. I've also installed BIND9 as a
caching DNS server.

Some services I'm not bothering to migrate:

 * Subversion, I've switched to github and migrated anything I cared about out of
   subversion. It was useful at the time I set it up 6 or so years ago, but other/better
   options exist now.
 * MediaWiki, most of the stuff in there was either related to a project in Subversion.
   So I'll switch to using the github wiki associated with a project. No need to worry
   about security or keeping software up to date.

So all in all a success switching to Ubuntu.

Now onto my wish list:

 * Ruby/Rails using passenger and Postgres, for messing about with personal projects.
 * Virtual hosts in Apache or even switch to nginx
 * Monitoring system stats like Memory/CPU etc
 * DNS within home network so I can setup aliases like dns.home
