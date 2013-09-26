---
layout: post
title: "Ember.js and Google Analytics"
date: 2013-09-17 11:07
comments: true
categories: emberjs
---


## Update

As some people have pointed out in the comments the code I gave has a bug in it.
The `observes('currentPath')` on an application controller doesn't fire when you
transition between paths in the same route. eg '/lesson/1/activity/1' to '/lesson/1/activity/2'

A better approach is to add the code to your router.

{% codeblock lang:coffeescript %}

App.Router.reopen

  didTransition: (infos) ->
    @_super(infos);
    return unless window.ga
    Em.run.next ->
      ga('send', 'pageview', {
         'page': window.location.hash,
         'title': window.location.hash
      });

{% endcodeblock %}

Matthew Beale also pointed out below that there may be a new API coming on the
router to achieve the same result.
[PR#3452](https://github.com/emberjs/ember.js/pull/3452) and
[PR#3453](https://github.com/emberjs/ember.js/pull/3453)

------------------------------------------

Single Page Javascript Applications and specifically Ember.js applications don't
always expose the right information to Google Analytics. Usually you'll see
100% traffic to `/` and nothing else.

What I wanted to be able to see what areas of the site people were using in real time.
A bit of searching led to various solutions with
[this post](http://www.pansapien.com/ember/2013/01/using-google-analytics-with-ember-js/)
having the most upto date solution. Working in Ember you quickly become wary
of older solutions posted and start filtering search results based on time.
Hopefully now that 1.0 is out the correct solutions will start bubbling to the
top of Google searches.

To make your Ember.js application more visible to Google Analytics you need to
expose the value of your application state. I'm using the hash
scheme (eg `#/album`) so whatever is after the hash in the url shows where a
person is in the application.

First you'll need to have the Google Analytics Javascript library loaded, I've
been using the more recent
[analytics.js](https://developers.google.com/analytics/devguides/collection/analyticsjs/)
library which has a different API to the older ga.js library. After you've
followed Google's instructions the library is available at `ga`.

Next you want to observe whenever the hash path changes within your app. The
code below assumes you're using hashes rather than HTML5 pushState.

{% codeblock lang:coffeescript %}
App.ApplicationController = Em.Controller.extend

    routeChanged: (->
        return unless window.ga
        Em.run.next ->
            ga('send', 'pageview', {
                'page': window.location.hash,
                'title': window.location.hash
                });
    ).observes('currentPath')

{% endcodeblock %}

The `routeChanged` function gets called when `currentPath` changes; it checks
whether `ga` is defined and if it is it sends a pageview event with the current
value of location.hash. The `Em.run.next` is there to make sure all the routing
has occured and the hash value is final before using it.

`page` and `title` are just strings so you could provide meaningful formatting
based on what your application does. The next obvious step is to add
[event tracking](https://developers.google.com/analytics/devguides/collection/analyticsjs/events)
to really see how people interact with the app.
