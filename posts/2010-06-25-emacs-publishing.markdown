---
title: .emacs publishing
author: Tim McGilchrist
date: 2010-06-25 00:00
tags: emacs
description: .emacs publishing
---

I signed up for a BitBucket account earlier this month but I've only recently
got around to using it. My first public project is my .emacs file. Check it out
at [lambda_foo/.emacs](http://bitbucket.org/lambda_foo/.emacs).

Includes partially working setup for:

 * CommonLisp and SLIME
 * Erlang/OTP
 * Ruby
 * Haskell
 * Groovy

The whole config is split into individual lisp files that are specific to a
language or cross cutting feature. The inspiration for doing this work came
from [M-x all-things-emacs](http://www.emacsblog.org/2007/10/07/declaring-emacs-bankruptcy/)
