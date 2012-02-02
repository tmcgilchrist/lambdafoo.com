---
layout: post
title: Thoughts on ObjectiveC and Cocoa
categories:
- ObjectiveC
- OS X
---
I wanted to collect my initial impressions of ObjectiveC and Cocoa. I've started
reading the Hillegas book again after and interrupted start last year with the
goal of doing a lot more osx coding in the future.

The things I'm enjoying most about the language are:

* the message passing nature of the method calls. It just seems like the right
   thing to do especially after experiencing Erlang and Ruby which both feature
   message passing.
 * the Cocoa APIs are beautifully self documenting and often read like pseudo
   code.
 * the fantastic use of Design Patterns within Cocoa, you can really see the
   benefits I using things like Delegates after spending some time doing ObjC.

The downsides so far are limited to manual memory management within
ObjC. Granted there is a garbage collector on OSX but not on iPhones etc, so the
common wisdom seems to be you need to know how to use both. I don't see this
will be an insurmountable problem as I managed to learn it for C++ which was a
much larger language.

The Hillegas book I'm using is brilliant but what would you expect from a guy
that has run training course on the subject for so long. The book does feel like
running through a training course but one run by a guy genuinely exciting about
the subject. My only wish is that there was an equivalent for the iPhone /
iPad.
