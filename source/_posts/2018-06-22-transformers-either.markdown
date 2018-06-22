---
layout: post
title: "On EitherT"
date: 2018-06-22 00:00
comments: true
categories:
 - haskell
---
In choosing Haskell as a language you sign up for a certain class of features and behaviours.
e.g. lazy evaluation, static typing

This gives you a general point in the design space for general prupose langauages but like
all languages you are still left with a number of choices in building software. These choices
are broad, diverse and hotly debated, sometimes they get labelled with **Best Practices** or the
**Right way**. Like any good engineer you should recognise that everything involves
trade-offs and that these labels are trying to hide that. There is not always one best way, an
approach has positives and negatives. Knowing those trade offs and deliberately choosing an
approach based off them is good engineering.

In programming language communities there are always bikeshedding arguments and Haskell is no different.
I want to call out a particular point of view around using exceptions vs data types in
Haskell when dealing with *errors*. Both are valid design points in a wider error handling design
space. The exception path is widely associated with Snoyman, who has written much software and
written extensively about this in [Exceptions Best Practices in Haskell](https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell)
and in the [Safe Exceptions](http://hackage.haskell.org/package/safe-exceptions) package.

I'd like to highlight the negatives, as I see them, of that approach and suggest a different set
of trade offs around modelling *errors* as data types using EitherT/ExceptT.

EitherT is a Monad Transformer built on the familar Either data type.

{%codeblock lang:haskell%}
data Either a b
  = Left a
  | Right b
{% endcodeblock %}

where typicall `Left` represents some failure case in this context and `Right` represents success.
Another formulation from OCaml community is:

{%codeblock lang:ocaml%}
type ('a,'b) result
  = Ok of 'a
  | Error of 'b
{% endcodeblock %}

which is more explicit about what the two constructors represent.

Async Exceptions
---------------

Back in the beginning, actually in 2000/01, asynchronous exceptions were added to Haskell. [2]
Quoting Simon Marlow:

 > Basically it comes down to this: if we want to be able to interrupt purely functional code,
 > asynchronous exceptions are the only way, because polling would be a side-effect.

So Haskell has async exceptions whether you like them or not, the ship has sailed.
This means that any code in `IO` can throw a runtime exception, further any thread can receive an
async exception.

So, how should we best deal with this reality and structure our code?

Exceptions
---------------

We have exceptions lets use them.

To start doing that you need to define your own custom exception type.

{%codeblock lang:haskell%}
data VapourError =
    InsufficientFunds
  | ItemUnavailable Text
  | MachineMalfunction Text
  deriving Typeable

-- Write a reasonable Show instance for each error
instance Show VapourError where
  show a = case a of
    InsufficientFunds -> "Insufficient funds."
    ItemUnavailable i -> "Item " ++ i " unavailable."
    MachineMalfunction e -> "Hardware malfunction " ++ e ++ "."

instance Exception VapourError
{%endcodeblock%}

The three steps we need are:


 1. Build the custom error type as a data type
 2. Provide a show instance, this could be generated but your error messages would not be great.
 3. Make your custom error an instance of `Exception`

At this point you can use `throw`, `catch` and `handle` with your custom error type.

{%codeblock lang:haskell%}
runVendingMachine :: VendingMachineState -> Coin
                   -> IO Product
runVendingMachine state coin = do
  unless (coin > 0) $ throw InsufficientFunds
  dispenseItem state coin


dispenseItem :: VendingMachineState -> Coin
             -> IO VendingMachineState
dispenseItem = ....
{%endcodeblock%}

Looking at the signature of `runVendingMachine` you can see that it returns a `Product` by running a
computation in `IO`. The problem you have when looking at that code is the signature doesn't
give you any indication that it might fail outside of the `IO` which we saw earlier can fail with anything.
So as a consumer of this function, how are you to know what exceptions to catch. Your options are:

 * Catch all exceptions - clearly dangerous and wrong
 * Catch a subset of exceptions - better but tricky to do correctly

The first option is dangerous as catching all exceptions includes asynchronous exceptions like
stack/heap overflow, thread killed and user interrupt. The documentation in `safe-exception` is
particularly helpful here and I recommend you read it thoroughly, it is well written. The short version is
you should only catch certain exceptions, trying to handle `StackOverflow`, `HeapOverflow` and `ThreadKilled`
exceptions could cause your program to crash or behave in unexpected ways.

The second option is error prone. The process for finding the possible exceptions involves reading the source code
and reading the haddock docs, with the goal of finding the set of sensible exceptions you need to put into
a `catch` or `handle` call. Have you found all the places an exception might be thrown? What about if you
pull in a new dependency, does it throw exceptions? What about a sub-dependency of a dependency?

What about the functions `runVendingMachine` calls? And their functions? To me it feels like going
back to Javascript or Ruby land and giving up on some of the benefits of a typed language. I want the types to
help me find the places I need to consider the errors, just like pattern matching does for data types.

The other less obvious (perhaps) issue is that you force the consumers of your function to know all
the gory details of exceptions in Haskell, which ones are safe to catch and what to do. Getting this right is
hard and tricky, and really belongs in a library so that it can be written one and reused.

Finally the behaviour of a Haskell system in production is such that throwing an exception would yield you exactly
what the show instance for `VapourError` is. It wouldn't give you a classic stack trace (unless you set that up)
so you loose context where the exception was raised and what was happening around it. At a previous workplace we
spend many weeks tracking down SSL and connection reset exceptions that occured in a base library but bubbled
out through multiple layers of application code. It wasn't fun.

This style is perfect for a quick script to munge some data, or an ICFP programming contest

If you really need exceptions, use `bracket` pattern or `safe-exceptions` like library. Keep the
complexity contained and code needs to be written *very* carefully.

Data Types
---------------

We mentioned data types earlier, using data types to model your computation is the natural approach in Haskell.
You build a data type that accurately reflects the data or states that you want to model. We even
did it for the custom `VapourError` type earlier.

Extending that we will use a particular data type `EitherT` to model errors. This is a `monad transformer` with
an `Either` where the monad could be anything.

In context it would look something like:

{%codeblock lang:haskell%}
crankHandle :: Int -> EitherT VapourError IO Product
-- or
crankHandle :: Monad m => Int -> EitherT VapourError m Product
-- or
crankHandle :: MonadIO m => Int -> EitherT VapourError m Product
{%endcodeblock%}

The type of our error is present in the type of our function, a familar situation.
If the monad `m` isn't IO then we have a good degree of confidence that
none of the base `exceptions` will be present.

Solution
---------------

{%codeblock lang:haskell%}

# Build a data type that represents the possible error states
data VapourError =
    InsufficientFunds
  | ItemUnavailable Text
  | MachineMalfunction Text

# Provide a function for turning errors into text
renderVapourError :: VapourError -> Text
renderVapourError = ...

# Usage site
runVendingMachine :: VendingMachineState -> Coin -> EitherT VapourError IO Product
runVendingMachine = ...
{% endcodeblock %}

Either - Examples
---------------

Examples of substantial pieces of code using `EitherT` to organise errors.

 * mafia - https://github.com/haskell-mafia/mafia/search?utf8=✓&q=EitherT&type=
 * boris - https://github.com/markhibberd/boris/search?utf8=✓&q=EitherT&type=
 * traction - https://github.com/markhibberd/traction/search?utf8=✓&q=EitherT&type=
 * mismi - https://github.com/nhibberd/mismi/search?q=EitherT&type=Code&utf8=✓

Either Advantages
---------------

 * function signatures clearly indicate error states
 * exhaustive pattern matching indicates where errors have/have not been handled
 * requires explicit composition of error data types

Basically the compiler helps you handle the various states required using the type system.

Exception - Examples
---------------

Example of code using `Exceptions` to organise errors

 * http-client - using non-200 response codes as exceptions
 * stack - internally follows an exception style

Exception Disadvantages
---------------

The main downsides as I see it to exception oriented code are:

 * exception throwing functions compose too easily forced to think about what it means.
 * no stack traces by default in Haskell mean you lose context.
 * handling exceptions requires knowledge about the internals of dependencies and how they use exceptions.

Here the compiler is less helpful in guiding you, giving little or no help with handling particular exceptions
or giving compile errors for new exceptions that you might need to consider.

Supporting Libraries
---------------

The supporting libraries for this pattern of error handling are:

 * transformers-either - Provides a type alias `type EitherT = ExceptT` plus addition operators.
 * transformers-bifunctor - Provies bifunctors over a monad transformer.

There is nothing revolutionary about `transformers-either`, you could roll your own version
easily or use the `ExceptT` transformer provided in the `transformers` package (adding any helper
functions you need). The value codes in a structured, consious handling of errors and using the
Haskell compiler to help.

Conclusion
---------------

> The primary value of avoiding exceptions is that it makes error behavior explicit in the
> type of the function. If you’re in an environment where everything might fail, being explicit
> about it is probably a negative. But if most of your function calls are total, then knowing
> which ones might fail highlights places where you should consider what the correct behavior
> is in the case of that failure. Remember that the failure of an individual step in your program
> doesn’t generally mean the overall failure of your code.
>
> It’s a little bit like null-handling in languages without options. If everything might
> be null, well, option types probably don’t help you. But if most of the values you encounter
> in your program are guaranteed to be there, then tracking which ones might be null be tagging
> them as options is enormously helpful, since it draws your attention to the cases where it might
> be there, and so you get an opportunity to think about what the difference really is.

> - Yaron Minsky

References
---------------

 1. [Asynchronous Exceptions in Practice](https://simonmar.github.io/posts/2017-01-24/asynchronous-exceptions.html)
 2. [Asynchronous Exceptions in Haskell](https://simonmar.github.io/bib/papers/async.pdf)

 3. [Exceptions Best Practices in Haskell](https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell)
 4. [The RIO Monad](https://www.fpcomplete/com/blog/2017/07/the-rio-monad)
 5. [Yaron's Thoughts](https://discuss.ocaml.org/t/specific-reason-for-not-embracing-the-use-of-exceptions-for-error-propagation/1666/15)
 6. [Checked Exceptions](https://www.well-typed.com/blog/2015/07/checked-exceptions/)

Links
---------------
https://hackage.haskell.org/package/transformers-either-0.1.0/docs/Control-Monad-Trans-Either.html
http://hackage.haskell.org/package/base-4.11.0.0/docs/System-IO.html#t:IO
http://hackage.haskell.org/package/base-4.11.0.0/docs/Control-Exception-Base.html
