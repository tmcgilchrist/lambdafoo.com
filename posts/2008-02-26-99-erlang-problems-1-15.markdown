---
title: "99 Erlang Problems: 1 - 15"
author: Tim McGilchrist
date: 2008-02-26 00:00
tags:
- Coding
- Erlang
description: "99 Erlang Problems: 1 - 15"
---
Following in the tradition of 99 Lisp/Haskell/Prolog problems, I present my
attempt at 99 Erlang problems. I'm writing these while I work my way through
the Progamming Erlang book. They are great short exercises and actually doing
them forces me to get my head around Erlang, rather than reading pages of code
and not really getting it. I'd definitely recommend doing these exercises as
you read the Erlang book, most of these are from my train commute.

Before you start reading the code please note that I've never done any Erlang
coding before doing these exercises, so they have come out as very Prolog-ish as
that was the most recent Functional language I used. If you find any bugs please
email me or comment on this article.

Answers for Problems 1 to 15 as adapted from the original Lisp version.

``` erlang

%%%  Ninety-Nine Erlang Problems
-module(problems).
-compile(export_all).

%%% P01 Find the last element of a list.
%%% eg
%%%   p01([1,2,3,4])
%%%   > 4
p01([]) -> [];
p01([H|[]]) -> H;
p01([_|T]) ->
    p01(T).

%%% P02 Find the last but one box of a list.
%%% eg
%%%    problems:p02([1,2,3,4]).
%%%    > [3,4]
p02([X,Y|[]]) ->
    [X,Y];
p02([_|Y]) ->
    p02(Y);
p02([]) ->
    [].

%%% P03 Find the K'th element of a list
%%% eg
%%%     problems:p03([1,2,3], 3).
%%%     > 3
p03([], _) ->
    [];
p03([H|_], 1) ->
    H;
p03([_|T], K) ->
    K1 = K - 1,
    p03(T, K1).

%%% P04 Find the number of elements of a list.
%%% eg
%%%     problems:p04([1,2,3,4,5]).
%%%     > 5
p04([]) ->
    0;
p04([_|T]) ->
    1 + p04(T).

%%% P05 Reverse a list.
%%% eg
%%%     problems:p05([1,2,3,4]).
%%%     > [4,3,2,1]
p05(List) ->
    p05(List, []).

p05([H|T], Rev) ->
    p05(T, [H|Rev]);
p05([], Rev) ->
    Rev.

%%% P06 Find out whether a list is a palindrome.
%%% eg
%%%     problems:p06([1,2,3,2,1]).
%%%     > true
p06(List) ->
    Other = p05(List),
    compare(Other, List).

compare([H|T], [H|T1]) ->
    compare(T, T1);
compare([H|_], [H1|_]) ->
    if H =/= H1 ->
	    false
    end;
compare([],[]) ->
    true.

%%% P07 Flatten a nested list structure
%%% eg
%%%     problems:p07([a,[b,[c,d],e]]).
%%%     > [a,b,c,d,e]
p07([H|T]) ->
    if
	is_list(H) ->
	    lists:append(p07(H), p07(T));
	true ->
	    [H|p07(T)]
    end;
p07([]) -> [].

%%% p08 Eliminate consecutive duplicates of list elements.
%%% eg
%%%     problems:p08([a,a,a,a,b,c,c,a,a,d,e,e,e,e).
%%%     > [a,b,c,d,e]
p08([]) ->
    [];
p08([H|[]]) ->
    [H];
p08([H,H|T]) ->
    p08([H|T]);
p08([H1,H2|T]) ->
    if
	H1 =/= H2 ->
	    [H1|p08([H2|T])]
    end.

%%% p09 Pack consecutive duplicates of list elements into sublists.
%%% eg
%%%     problems:p09([a,a,a,a,b,c,c,a,a,d,e,e,e,e]).
%%%     > [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]
p09([]) ->
    [];
p09([H|[]]) ->
    [[H]];
p09([H,H|C]) ->
    [Head|Tail] = p09([H|C]),
    X = lists:append([H],Head),
    [X|Tail];
p09([H,H2|C]) ->
    if H =/= H2 ->
	    [[H]|p09([H2|C])]
    end.


%%% p10 Run-length encoding of a list.
%%% eg
%%%     problems:p10([a,a,a,a,b,c,c,a,a,d,e,e,e,e]).
%%%     > [[a,4],[b,1],[c,2],[a,2],[d,1],[e,4]]
p10(List) ->
    M = p09(List),
    encode(M).
encode([]) ->
    [];
encode([H|T]) ->
    if
	is_list(H) ->
	    [H2|_] = H,
	    [[H2,length(H)]|encode(T)];
	true ->
	    [[H,1]|encode(T)]
    end.

%%% p11 Modified run-length encoding
%%% eg
%%%     problems:p10([a,a,a,a,b,c,c,a,a,d,e,e,e,e]).
%%%     > [[a,4],b,[c,2],[a,2],d,[e,4]]
p11(List) ->
    M = p09(List),
    encode_mod(M).
encode_mod([]) ->
    [];
encode_mod([H|T]) ->
    if
	is_list(H) ->
	    [H2|_] = H,
	    Length = length(H),
	    case Length of
		1 ->
		    [H2|encode_mod(T)];
		_ ->
		    [[H2,Length]|encode_mod(T)]
	    end
    end.


%%% p12 Decode a run-length encoded list.
%%% eg
%%%     problems:p12([[a,4],b,[c,2],[a,2],d,[e,4]]).
%%%     > [a,a,a,a,b,c,c,a,a,d,e,e,e,e]
p12([]) ->
    [];
p12([H|T]) ->
    if
	is_list(H) ->
	    [Element,Number] = H,
	    lists:append(repeat(Element, Number), p12(T));
	true ->
	    lists:append([H], p12(T))
    end.

repeat(_, 0) ->
    [];
repeat(H, Number) ->
    [H|repeat(H, Number -1)].

%%% p13 Run length encoding of a list
%%% eg
%%%     problems:p13([a,a,a,a,b,c,c,a,a,d,e,e,e,e]).
%%%     > [[4,a],b,[2,c],[2,a],d,[4,e]]
p13([]) ->
    [];
p13(L) ->
    {Crumbs, Eaten} = chomp(L, []),
    [H|_] = Eaten,
    Length = length(Eaten),
    case Length of
	1 -> [H|p13(Crumbs)];
	_ -> [[length(Eaten), H]|p13(Crumbs)]
    end.

chomp([], R) ->
    {[], R};
chomp([H|T], []) ->
    chomp(T, [H]);
chomp([H|T], [HR|Result]) ->
    if H =:= HR ->
	    chomp(T, [H,HR|Result]);
       true ->
	    {[H|T], [HR|Result]}
    end.

%%% p14 Duplicate elements of a list.
%%% eg
%%%     problems:p14([a,b,c,d]).
%%%     > [a,a,b,b,c,c,d,d]
p14([]) ->
    [];
p14([H|T]) ->
    [H,H|p14(T)].

%%% p15 Replicate the elements of a list a given number of times.
%%% eg
%%%     problems:p15([a,b,c],3).
%%%     > [a,a,a,b,b,b,c,c,c]
p15([], _) ->
    [];
p15([H|T], Number) ->
    lists:append(repeat(H, Number), p15(T, Number)).

```
