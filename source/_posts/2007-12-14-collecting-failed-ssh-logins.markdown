---
layout: post
title: Collecting Failed SSH Logins
categories:
- Solaris
---
It's always a good idea to keep an eye on what's happening on your servers,
especially when they're connected to a hostile environment like the
Interweb. Following in this vein, I'm going to show how I configured my Solaris
server to collect failed login attempts.

First, you'll need a root shell and then open up the sshd configuration file:

    vi /etc/ssh/sshd_config

Find the line containing 'LogLevel'. On my machine it looked like

    LogLevel    info

We need to change the info to debug, so it should look like this:

    LogLevel    debug

Save that file and open up the one for syslog

    vi /etc/syslog.conf

Go to the bottom of that file and add the following line:

    auth.debug                      ifdef(`LOGHOST', /var/log/authlog, @loghost)

This should direct the sshd debug messages, which includes failed login attempts
to a new logfile **authlog** under **/var/log**. One thing to be careful of is
the spacing needs to be tabs not spaces otherwise it wont work.

Now we need to restart both services so they pickup the changes in
configuration, then we'll test that it all works.

Restart syslog:

    svcadm restart svc:/system/system-log:default

and to restart sshd:

    svcadm restart svc:/network/ssh:default

Just check that everything started correctly, if you type

    svcs -x

you should see no errors, alternatively you could type it without the '-x' which
shows all the started services and the restarted ones will appear at the end of
the list.

Now you can test it works by tailing the logfile **/var/adm/authlog** in one
terminal and attempting to login via another. Hopefully now it all works and you
see the failed login attempt in authlog. One option piece of configuration I've
added is to rotate the authlog, this isn't strictly necessary but it does clean
things up a bit.

I decided on rotating the logs every month and keeping 4 months worth of logs
apart from the current log.

    logadm -w /var/log/authlog -C 4 -p 1m -t '$dirname/$basename-%m-%Y.log'

If you want to keep more logs, change the 4 to something more or less and the
time period before rotating the logs is after the **-p** switch so you can change
that to 6m for 6 months or 1w for 1 week. Have a look at the manpage for logadm
for more details.

The only extra thing you might want to add is compression of the logfile after
rotating, or processing the logfile to give you a summary of activity. I've been
looking into the summary but haven't found anything that fits my needs.
