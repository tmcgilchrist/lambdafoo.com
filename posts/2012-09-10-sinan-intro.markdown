---
title: Getting started with Sinan
author: Tim McGilchrist
date: 2012-09-10 21:00
tags: Erlang Sinan
description: Getting started with Sinan
---

## Background

When I started with Erlang I used a simple Makefile to call `erlc` and pretty
much did things by hand. After a number of years in the wilderness I found out
about [rebar](http://github.com/basho/rebar) from Basho and started using that
to compile my code. Everything was good, rebar knew how to compile OTP apps and pull down external
dependencies. Except when I needed to generate releases. It's
not necessarily straight forward to get rebar to build you a nice release,
[not that it's impossible](http://www.metabrew.com/article/erlang-rebar-tutorial-generating-releases-upgrades)
it's just not as simple as I'd like.

Enter Sinan, the somewhat forgotten erlang build tool.

Sinan is a build tool designed to build Erlang/OTP projects, releases and
applications. It claims to be more OTP than rebar and uses the OTP metadata
artefacts to build your project with little configuration needed.

Let's see how well it delivers on the promise.

## Sinan From Scratch

First you'll need Erlang installed, which your friendly local package management tool
should provide. I'm using Homebrew on OSX so I just did:

``` shell
$ brew install erlang
...
$ erl -v
Erlang R15B01 (erts-5.9.1) [source] [64-bit] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.9.1  (abort with ^G)
1>
```
Linux should be similarly straight forward and Windows well you're on your own.

Grab sinan from the
[downloads page](https://github.com/erlware/sinan/downloads) on github, I'm
using version 4.1.1. Put it somewhere on your PATH, I've got mine in ~/bin which is on my PATH, and chmod
+x it so it's executable.

Now for the fun bit, type `sinan gen` and fill in the details.

``` shell

Please specify your name
your name> Tim McGilchrist
Please specify your email address
your email> timmcgil@gmail.com
Please specify the copyright holder
copyright holder ("Tim McGilchrist")>
Please specify name of your project
project name> sinan_demo
Please specify version of your project
project version> 0.0.1
Please specify the ERTS version ("5.9.1")>
Is this a single application project ("n")> y
Would you like a build config? ("y")> y
Project was created, you should be good to go!

```

From that Sinan has generated a project, filling in your details, with an OTP
application and some build configuration. Your directories should look something
similar to this.

``` shell

sinan_demo
  |-- config
  |    |-- sys.config
  |-- doc
  |-- ebin
  |    |-- overview.edoc
  |-- include
  |-- sinan.config
  |-- src
  |    |-- sinan_demo.app.src
  |    |-- sinan_demo_app.erl
  |    |-- sinan_demo_sup.erl

```

It includes all the standard directories you'd expect plus a `sinan.config`
file.

First a little diversion, we need to add a line to the sinan config file, which
tells sinan to include the erlang runtime system when it generates a release.
Open sinan.config and add `{include_erts, true}.` as the last line. It should
look like this:

``` erlang

{project_name, sinan_demo}.
{project_vsn, "0.0.1"}.

{build_dir,  "_build"}.

{ignore_dirs, ["_", "."]}.

{ignore_apps, []}.

{include_erts, true}.

```

Back to making our generated code runnable.

By default the generated supervisor doesn't point to a valid module so you'll
need to remedy that before trying to startup the application. Create a new file
called `sinan_demo_server.erl` in `src` and drop the following code in.

``` erlang

-module(sinan_demo_server).

-behaviour(gen_server).

%% API
-export([start_link/0, add_one/0, total/0]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {count}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

total() ->
    gen_server:call(?MODULE, total).

add_one() ->
    gen_server:call(?MODULE, add).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([]) ->
    io:format("starting~n", []),
    {ok, #state{count = 0}, 0}.

handle_call(add, _From, State) ->
    NewCount = State#state.count + 1,
    NewState = State#state{count = NewCount},
    Reply    = {ok, NewState},
    {reply, Reply, NewState};
handle_call(total, _From, State = #state{ count = Count }) ->
    {reply, Count, State};
handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

```


It's a pretty standard OTP gen_server application with 2 API
methods. `add_one/0` adds 1 to the counter and `total/0` returns the value of the
counter. The record definition setups up the state record for this server with
just a `count` attribute. The 2 API functions use the `gen_server:call/2` method
to hit the OTP callback for `handle_call/3`.

Next we need to fix the supervisor so it starts the correct module. Change
`sinan_demo_sup.erl` so it looks like the code below:

```erlang

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
-spec init(list()) -> {ok, {SupFlags::any(), [ChildSpec::any()]}} |
                            ignore | {error, Reason::any()}.
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    AChild = {sinan_demo_server, {sinan_demo_server, start_link, []},
              Restart, Shutdown, Type, [sinan_demo_server]},

    {ok, {SupFlags, [AChild]}}.

```

The 2 changes we have make is to `start_link/0` so we can call the server
directly, and fix the child spec so it starts our new module.

Now we need to add the sinan_demo_server module to `sinan_demo.app.src` so we
know about it when generating the OTP application. Just add it to the list of
modules like so:

```erlang

%% This is the application resource file (.app file) for the,
%% application.
{application, sinan_demo,
 [{description, "Sinan demo application."},
  {vsn, "0.0.1"},
  {modules, [sinan_demo_app,
             sinan_demo_sup,
             sinan_demo_server]},
  {registered,[sinan_demo_sup]},
  {applications, [kernel, stdlib]},
  {mod, {sinan_demo_app,[]}},
  {start_phases, []}]}.

```

Compile with `sinan build` and hopefully everything works.

From here you've got a few options to get your application running,
but the easiest is just to use the sinan shell and start your application from
there.

``` shell

$ sinan shell
Eshell V5.9.1  (abort with ^G)
1> application:which_applications().
[{parsetools,"XLATETOOLS  CXC 138 xx","2.0.7"},
 {syntax_tools,"Syntax tools","1.6.8"},
 {compiler,"ERTS  CXC 138 10","4.8.1"},
 {getopt,"Command-line options parser for Erlang","0.4.2"},
 {erlware_commons,"Additional standard library for Erlang",
                  "0.6.1"},
 {stdlib,"ERTS  CXC 138 10","1.18.1"},
 {kernel,"ERTS  CXC 138 10","2.15.1"}]
2> application:start(sinan_demo).
ok

```

We've started a shell and checked what applications are started with
`application:which_applications()`. Now start the demo application with:

``` shell
3> application:start(sinan_demo).
ok
```

Now lets test that we can call the application.

``` shell
4> sinan_demo_server:add_one().
{ok,{state,1}}
5> sinan_demo_server:add_one().
{ok,{state,2}}
```

As you can see it's calling the server and incrementing the call count.

The next step is to create a release, which is as simple as running `sinan release`

Sinan has created a number of new directories under `_build`

``` shell

$ tree -d _build/
    _build/
    `-- sinan_demo
        |-- bin
        |-- erts-5.9.1
        |-- lib
        |   |-- kernel-2.15.1
        |   |-- sinan_demo-0.0.1
        |   `-- stdlib-1.18.1
        `-- releases
            `-- 0.0.1

```

The `bin` directory is still there from last time but now we have an erts, lib
and releases directories. erts is there because earlier we asked sinan to
include an erlang runtime, so you can copy everything under _build/sinan_demo to
another machine without erlang installed and run this application. The
limitation being that the CPU and OS needs to match the machine you've built
on. `lib` includes all the applications you asked sinan to include, they'll
match what you have in your collective .app.src files. `releases` contains
configuration files specific to a particular release of the application.

Starting the release generated is as simple as

``` shell
$ ./_build/sinan_demo/bin/sinan_demo
Erlang R15B01 (erts-5.9.1) [source] [64-bit] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false]

starting
Eshell V5.9.1  (abort with ^G)
1> application:which_applications().
 [{sinan_demo,"Sinan demo application.","0.0.1"},
  {stdlib,"ERTS  CXC 138 10","1.18.1"},
  {kernel,"ERTS  CXC 138 10","2.15.1"}]
2>

```

We'll leave it there for now, but if you're curious like me you'll probably have
a bunch of questions of where to take sinan next.

Next time I'm going to cover:

 * Generating Version 2
 * Doing an OTP upgrade to Version 2
 * Downgrading to Version 1

But if you've got other suggestions please leave them in the comments.
