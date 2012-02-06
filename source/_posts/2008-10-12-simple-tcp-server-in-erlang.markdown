---
layout: post
title: Simple TCP Server in Erlang
categories:
- Coding
- Erlang
---
Just a quick example of a tcp server that returns the current time.

{% codeblock lang:erlang %}
-module(daytime).
-export([start/0]).

start() ->
    {ok, Listen} = gen_tcp:listen(1313, [binary,
                                        {packet, 0}, {active, false},
                                        {reuseaddr, true}]),
    spawn(fun() -> connect(Listen) end).

connect(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> connect(Listen) end),
    gen_tcp:send(Socket, getDateTime()),
    gen_tcp:close(Socket).

getDateTime() ->
    {YearMonthDay, {Hour, Minute, Second}} = calendar:universal_time_to_local_time(erlang:universaltime()),
    DayOfWeek = calendar:day_of_the_week(YearMonthDay),
    DayName = httpd_util:day(DayOfWeek),
    MonthName = httpd_util:month(Month),
    DateString = io_lib:format("~s, ~s ~p, ~p ~p:~p:~p~n",
                                         [DayName, MonthName,
                                         Day, Year,
                                         Hour, Minute, Second]),
    lists:flatten(DateString).

{% endcodeblock %}

To test, startup an erlang shell:

    erl

Compile the daytime.erl file:

    1> c(daytime).
    {ok,daytime}

Startup the daytime server:

    2> daytime:start().
    <0.50.0>

Now in another terminal fire up telnet and connect to port 1313.

    telnet localhost 1313

You should get something like this:

    Trying ::1...
    telnet: connect to address ::1: Connection refused
    Trying fe80::1...
    telnet: connect to address fe80::1: Connection refused
    Trying 127.0.0.1...
    Connected to localhost.
    Escape character is '^]'.
    Sun, Oct 12, 2008 11:33:26
    Connection closed by foreign host.
