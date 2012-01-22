--- 
layout: post
title: Erlang hot code loading
tags: 
- Coding
- Erlang
status: publish
type: post
published: true
meta: 
  _edit_last: "1"
  _aioseop_title: Example of Erlang hot code loading
  _aioseop_keywords: erlang hot code loading, hot code swap, erlang dynamic code loading,dynamic code loading
---
Nearly every posting about Erlang that you come across mentions the hot code loading feature, aka dynamic code loading. But so far I have had problems finding a simple example of how to add this to my code.

Starting from the wikipedia <a href="http://en.wikipedia.org/wiki/Erlang_(programming_language)#Hot_code_loading_and_modules">page</a>, here is a demonstration of how to do it.

Copy this code into an editor. I'm using Emacs here, so all instructions use it's keybindings.

<pre>
<code>
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
</code>
</pre>

Compile and start a new erlang shell, Ctrl-C Ctrl-K.

Now we want to start a new process running this code. Enter the following code into the erlang shell.

<pre>
2> Pid = spawn(fun() -> counter:start() end).
<0.37.0>
</pre>

Send the process a message to make sure it's alive and working.

<pre>
3> Pid ! {increment, 3}.
{increment,3}
4> Pid ! {print}.
Sum is 3
{print}
</pre>

Now that we have the first version of the process running, add a new clause to the receive like so.
<pre>
<code>
        {print} ->
	    io:format("Sum is ~p~n", [Sum]),
	    loop(Sum);
	{reset} ->
	    loop(0);
        {counter, Pid} ->
            Pid ! {counter, Sum},
            loop(Sum);
</code>
</pre>
Compile this version, Ctrl-C Ctrl-K.

Send a message to the process that only the new version can handle.

<pre>
6> Pid ! {reset}.
Unhandled message {reset}
{reset}
</pre>

So we are still running the previous version of the code.

<pre>
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
</pre>

The 'code_switch' message makes the process load the new version of code. So using this you can upgrade the running process without losing state. Cool.
