---
layout: post
title: "Erlang Hot Code Loading"
date: 2012-08-02 07:51
comments: true
categories:
    - Erlang
---

##

Erlang has a Code Server, that is responsible for hot code loading.

-> Look at some of the code for it.

Features:
    - Allows you to have 2 versions of a Module loaded.
    - External calls will automatically load the new version

Local Calls Vs Remote Calls

Local calls are to functions within the module
External calls specify a Module before the function.

    loop(S) ->
        myfun(),
        ?MODULE:myfun().

A third wheel?
What happens if I load a third Module.

Processes running the oldest version are killed, the assumption being that it
was orphaned or had no way to upgrade.
Otherwise the oldest version is simply dropped by the Code Server


Talk about the pitfalls/issues

-> changing the interface your modules export
