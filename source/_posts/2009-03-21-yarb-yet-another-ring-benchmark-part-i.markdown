--- 
layout: post
title: "YARB: Yet Another Ring Benchmark, Part I"
tags: 
- Coding
- Erlang
status: publish
type: post
published: true
meta: 
  _edit_last: "1"
---
In Joe Armstrong's excellent Erlang book he has an exercise to:
<blockquote>Write a ring benchmark. Create N processes in a ring. Send a message round the ring M times so that a total or N*M messages get sent. Time how long it takes for different values of N and M. Write a similar program in some other language you are familar with. Compare the results. Write a blog and public the results on the Internet!</blockquote>
So far I've done the Erlang section of this. It turns out to be pretty straight forward in Erlang, not surprisingly. If it was difficult it wouldn't be a great example now would it.

So lets see how it's done.
<pre>-module(ring).
-compile(export_all).
-import(lists, [foreach/2, map/2, reverse/1]).

main([A,B]) -&gt;
    N = list_to_integer(A),
    M = list_to_integer(B),
    io:format("~w: ~w processes with ~w messages\n", [self(), N, M]),</pre>
This section simply reads in the command-line arguments and uses <em>list_to_integer()</em> to convert them into numbers.
<pre>[First|_] = Pids = for(1, N, fun() -&gt; spawn(fun() -&gt; loop() end) end),
setupLoop(Pids),</pre>
Next we spawn the required number of threads and setup the loop, so that each thread knows the PID of the next thread in the loop. 
<pre>sendMsgs(M, First),
% Cleanup!!
map(fun(P) -&gt; P ! {exit} end, Pids).</pre>
SengMsgs does the work of sending messages around the loop. It starts with sending a <em>{relay}</em> message to the first thread in the loop and waits for a reply from the last thread in the loop. When it receives a message from the last thread, it decrements the message count and sends another <em>{relay}</em> message. Continuing until the message count reaches 0.
<pre>sendMsgs(0, First) -&gt;
    First ! {relay},
    receive
        {relay} -&gt;
            io:format("~w: Finished ~w loops.~n", [self(), 0])
    end;
sendMsgs(N, First) -&gt;
    First ! {relay},
    receive
        {relay} -&gt;
            sendMsgs(N-1, First)
    end.</pre>
To setup the loop, we take the first 2 PIDs sending the second PID to the first as a message indicating that it's the next thread in the loop. When we get down to the last PID, the next thread is the main thread.
<div>

The second language to compare against, needs to be something with a similar threading model with support for something similar to message passing. I'm looking at doing it with either Haskell or Lisp but I haven't worked out how to do the message passing just yet. It'll have to wait for PartII.

Hope this helps someone, and please ask questions if my explaination isn't clear. </div>
Full source code follows, simply copy the code and place into a file called <em>ring.erl</em>. Compile using this command <em>erlc ring.erl</em> and run the resulting program like so <em>erl -noshell -run ring main 1 2 -run init stop</em>
<pre>-module(ring).
-compile(export_all).
-import(lists, [foreach/2, map/2, reverse/1]).

%%% Write a ring benchmark. Create N processes in a ring. Send a
%%% message round the ring M times so that a total of N*M messages
%%% get sent. Time how long this takes for different values of N and M.

%%% Write a similar program in some other language you are familar with.
%%% Compare the results. Write a blog, and publish the results on the Internet!
%%% Compile: "erlc ring.erl" Run: "erl -noshell -run ring main 1 2 -run init stop"
main([A,B]) -&gt;
    N = list_to_integer(A),
    M = list_to_integer(B),
    io:format("~w: ~w processes with ~w messages\n", [self(), N, M]),
    % Spawn 'N' processes and link them together.
    [First|_] = Pids = for(1, N, fun() -&gt; spawn(fun() -&gt; loop() end) end),
    setupLoop(Pids),

    % Start sending messages.
    sendMsgs(M, First),

    % Cleanup!!
    map(fun(P) -&gt; P ! {exit} end, Pids).

setupLoop([P]) -&gt;
    P ! {next, self()};
setupLoop([P1,P2|Rest]) -&gt;
    P1 ! {next, P2},
    setupLoop([P2|Rest]).

sendMsgs(0, First) -&gt;
    First ! {relay},
    receive
        {relay} -&gt;
            io:format("~w: Finished ~w loops.~n", [self(), 0])
    end;
sendMsgs(N, First) -&gt;
    First ! {relay},
    receive
        {relay} -&gt;
            sendMsgs(N-1, First)
    end.

loop() -&gt;
    io:format("~w: Starting node~w~n", [self(), self()]),
    receive
        {next, Pid} -&gt;
            loop(Pid);
        {exit} -&gt;
            io:format("~w: Exiting now! ~n", [self()])
    end.

loop(Pid) -&gt;
    receive
        {relay} -&gt;
            Pid ! {relay},
            loop(Pid);
        {exit} -&gt;
            io:format("~w: Exiting now!~n", [self()])
    end.

for(N, N, F) -&gt;
     [F()];
for(I, N, F) -&gt;
    [F() | for (I+1, N, F)].</pre>
