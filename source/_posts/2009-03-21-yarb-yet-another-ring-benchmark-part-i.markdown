---
layout: post
title: "YARB: Yet Another Ring Benchmark, Part I"
categories:
- Coding
- Erlang
---
In Joe Armstrong's excellent Erlang book he has an exercise to:

    Write a ring benchmark. Create N processes in a ring. Send a message round the
    ring M times so that a total or N*M messages get sent. Time how long it takes
    for different values of N and M. Write a similar program in some other language
    you are familar with. Compare the results. Write a blog and public the results
    on the Internet!

So far I've done the Erlang section of this. It turns out to be pretty straight
forward in Erlang, not surprisingly. If it was difficult it wouldn't be a great
example now would it.

So lets see how it's done.

{% codeblock lang:erlang %}
-module(ring).
-compile(export_all).
-import(lists, [foreach/2, map/2, reverse/1]).

main([A,B]) ->
    N = list_to_integer(A),
    M = list_to_integer(B),
    io:format("~w: ~w processes with ~w messages\n", [self(), N, M]),

{% endcodeblock %}

This section simply reads in the command-line arguments and uses
**list_to_integer()** to convert them into numbers.

{% codeblock lang:erlang %}

[First|_] = Pids = for(1, N, fun() -> spawn(fun() -> loop() end) end),
setupLoop(Pids),

{% endcodeblock %}

Next we spawn the required number of threads and setup the loop, so that each
thread knows the PID of the next thread in the loop.

{% codeblock lang:erlang %}

sendMsgs(M, First),
% Cleanup!!
map(fun(P) -> P ! {exit} end, Pids).

{% endcodeblock %}

SengMsgs does the work of sending messages around the loop. It starts with
sending a **{relay}** message to the first thread in the loop and waits for
a reply from the last thread in the loop. When it receives a message from the
last thread, it decrements the message count and sends another **{relay}**
message. Continuing until the message count reaches 0.

{% codeblock lang:erlang %}

sendMsgs(0, First) ->
    First ! {relay},
    receive
        {relay} ->
            io:format("~w: Finished ~w loops.~n", [self(), 0])
    end;
sendMsgs(N, First) ->
    First ! {relay},
    receive
        {relay} ->
            sendMsgs(N-1, First)
    end.

{% endcodeblock %}

To setup the loop, we take the first 2 PIDs sending the second PID to the first
as a message indicating that it's the next thread in the loop. When we get down
to the last PID, the next thread is the main thread.

The second language to compare against, needs to be something with a similar
threading model with support for something similar to message passing. I'm
looking at doing it with either Haskell or Lisp but I haven't worked out how to
do the message passing just yet. It'll have to wait for PartII.

Hope this helps someone, and please ask questions if my explaination isn't
clear.

Full source code follows, simply copy the code and place into a file called
**ring.erl**. Compile using this command **erlc ring.erl** and run the
resulting program like so

    erl -noshell -run ring main 1 2 -run init stop

{% codeblock lang:erlang %}

-module(ring).
-compile(export_all).
-import(lists, [foreach/2, map/2, reverse/1]).

%%% Write a ring benchmark. Create N processes in a ring. Send a
%%% message round the ring M times so that a total of N*M messages
%%% get sent. Time how long this takes for different values of N and M.

%%% Write a similar program in some other language you are familar with.
%%% Compare the results. Write a blog, and publish the results on the Internet!
%%% Compile: "erlc ring.erl" Run: "erl -noshell -run ring main 1 2 -run init stop"
main([A,B]) ->
    N = list_to_integer(A),
    M = list_to_integer(B),
    io:format("~w: ~w processes with ~w messages\n", [self(), N, M]),
    % Spawn 'N' processes and link them together.
    [First|_] = Pids = for(1, N, fun() -> spawn(fun() -&gt; loop() end) end),
    setupLoop(Pids),

    % Start sending messages.
    sendMsgs(M, First),

    % Cleanup!!
    map(fun(P) -> P ! {exit} end, Pids).

setupLoop([P]) ->
    P ! {next, self()};
setupLoop([P1,P2|Rest]) ->
    P1 ! {next, P2},
    setupLoop([P2|Rest]).

sendMsgs(0, First) ->
    First ! {relay},
    receive
        {relay} ->
            io:format("~w: Finished ~w loops.~n", [self(), 0])
    end;
sendMsgs(N, First) ->
    First ! {relay},
    receive
        {relay} ->
            sendMsgs(N-1, First)
    end.

loop() ->
    io:format("~w: Starting node~w~n", [self(), self()]),
    receive
        {next, Pid} ->
            loop(Pid);
        {exit} ->
            io:format("~w: Exiting now! ~n", [self()])
    end.

loop(Pid) ->
    receive
        {relay} ->
            Pid ! {relay},
            loop(Pid);
        {exit} ->
            io:format("~w: Exiting now!~n", [self()])
    end.

for(N, N, F) ->
     [F()];
for(I, N, F) ->
    [F() | for (I+1, N, F)].

{% endcodeblock %}
