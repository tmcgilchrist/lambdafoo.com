---
layout: post
title: Erlang hot code loading
published: false
categories:
- Erlang
---
Nearly every posting about Erlang that you come across mentions the hot code
loading feature, aka dynamic code loading. But so far I have had problems
finding a simple example of how to add this to my code. So here is one.

Copy this code into an editor. I'm using Emacs here, so all instructions use it's keybindings.

{% codeblock lang:erlang %}

%% A process whose only job is to keep a counter.
-module(counter).
-export([start/0, codeswitch/1]).

start() -> loop(0).

loop(Sum) ->
    receive
        {increment, Count} ->
            loop(Sum+Count);
	{print} ->
	    io:format("Sum is ~p~n", [Sum]),
	    loop(Sum);
        {counter, Pid} ->
            Pid ! {counter, Sum},
            loop(Sum);
        code_switch ->
            ?MODULE:codeswitch(Sum);
	M ->
	    io:format("Unhandled message ~p~n", [M])
    end.

codeswitch(Sum) -> loop(Sum).
{% endcodeblock %}

Compile and start a new erlang shell, Ctrl-C Ctrl-K.

Now we want to start a new process running this code. Startup and enter the
following code into the erlang shell.

    Erlang R15B (erts-5.9) [source] [64-bit] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false]

    Eshell V5.9  (abort with ^G)
    1> Pid = spawn(fun() -> counter:start() end).
    <0.37.0>

Send the process a message to make sure it's alive and working.


    3> Pid ! {increment, 3}.
    {increment,3}
    4> Pid ! {print}.
    Sum is 3
    {print}

Now that we have the first version of the process running, add a new clause to
the receive like so.

{% codeblock lang:erlang %}
        {print} ->
	    io:format("Sum is ~p~n", [Sum]),
	    loop(Sum);
	{reset} ->
	    loop(0);
        {counter, Pid} ->
            Pid ! {counter, Sum},
            loop(Sum);
{% endcodeblock %}

Compile this version, Ctrl-C Ctrl-K and send a message to the process that only
the new version can handle.

    6> Pid ! {reset}.
    Unhandled message {reset}
    {reset}

So we are still running the previous version of the code.

    6> Pid ! code_switch.
    code_switch
    7> Pid ! {print}.
    Sum is 3
    {print}
    8> Pid ! {reset}.
    {reset}
    9>  Pid ! {print}.
    Sum is 0
    {print}

The 'code_switch' message makes the process load the new version of code. So
using this you can upgrade the running process without losing state. Cool.
