---
title: OCaml CI with CircleCI
author: Tim McGilchrist
date: 2021-02-02 00:00
tags: ocaml ci
description: How to setup OCaml CI using CircleCI
---

I wanted to share a simple configuration for running OCaml projects in [CircleCI](https://circleci.com).
CircleCI is what I'm using at work plus it supports a killer feature
that you can re-run a failing build getting an SSH session into the machine. This one
feature has saved me loads of time in debugging CI configuration and flakey tests.
Most of the other [features](https://circleci.com/docs/) are similar to other cloud CI solutions,
the documentation is solid and setting up more advanced workflows is easy enough.

Our requirements are simple to build OCaml projects that use OPAM and have simple
test requirements (just running unit tests).

First we add a file `.circleci/config.yml` with:

``` yaml
version: 2
jobs:
  build-4.10:
    docker:
      - image: ocaml/opam2:4.10
    steps:
      - checkout
      - run:
          name: Build
          command: ./bin/ci

workflows:
  version: 2
  build:
    jobs:
      - build-4.10

```

This creates a job `build-4.10` using docker image `ocaml/opam2:4.10` published by the OCaml team.
The `steps` defines the commands to run, we use a built in `checkout` command provided by CirclCI and
then a `run` command that executes a shell script `./bin/ci`.

You could use your own docker container in place of `ocaml/opam2:4.10`, maybe pre-installing
some things or using a different linux distro. How to run the command could also be inlined
rather than being its own file. I chose to make it a file for two reasons, when you SSH to debug a
script you can just re-run `./bin/ci`, and you can re-use the steps between local and CI.

Now to the shell script

``` shell
#!/bin/sh -eux

WORKING_DIR=$(pwd)

# Install some extras
sudo apt-get install m4 -y

# Make sure opam is setup in your environment.
eval `opam config env`
opam update

# Install each package as a dev dependency
find . -type f -name '*.opam' | sort -d | while read P; do
  opam pin add -n "$(basename -s .opam ${P})" . -y --dev
  opam install --deps-only "$(basename -s .opam ${P})"  -y
  eval `opam config env`
done

# Run the builds and
dune build
dune runtest
```

This configuration is from a project with multiple `opam` files so we have a `find`
to locate all those files. One gotcha with this is it'll sort the file names which may not
match the dependency order, if that is the case you will need to explicitly list them.
If you have a single `opam` file then replace that with
the following (replacing `project-name` with your project name).

``` shell
opam pin add -n "project-name" . -y --dev
opam install --deps-only "project-name"  -y
```

Push that into your github main branch, then `Set up Project` in the circleci UI and
you should be off and building. From here the circleci docs can help with setting up
different builds based off branches. Adding other OCaml builds is as easy as duplicating
the `build-4.10` section in YAML, pointing it to another docker container like `4.08` and
adding the new build name to `workflows` under `jobs:`.

There's a working setup in my [ocaml-bitbucket](https://github.com/tmcgilchrist/ocaml-bitbucket) project.
Good luck!
