---
title: Ripple on Rails
author: Tim McGilchrist
date: 2012-05-22 16:59
tags: riak ruby nosql
description: Ripple on Rails
---

## Introduction ##

Motivation?? Checkout the RailsCasts episode on Mongoid

Started with the Seven DB in Seven weeks book.
Wanted to see how Riak would mix with Rails / Ruby.

## What is Riak ##

Riak is a distributed key-value store based on the Amazon Dynamo papers. Written
in Erlang by the people at Basho, it's an exciting addition to the crowded NoSQL
zoo.

 - Map Reduce
 - eventually consistent
 - read / write balancing


## Installing Riak ##

Installing Riak is pretty straight forward for development, if you're on a Mac
use the excellent Homebrew package and you'll get the latest 1.2 release.

    brew install riak

Similarly on Linux it should be a matter of invoking your local package manager
and you should be set. I'm assuming you're using a Mac here, cause that's what
I've got but Linux should work as well. And Windows, well use a VM :-)

## Ruby Environment

You'll need some form of Ruby installed, I'd recommend installing via rvm over
using your local ruby version. See rvm.io for details, it should simply be a
matter of running a command in your terminal.

Next we'll create a gemset and install rails into that.

    rvm --create use 1.9.3@ripple
    gem install rails

## Creating a new Rails Application with Ripple

Now that we have Riak installed we'll create a new Rails 3 application that uses
Riak. Continuing in the Rails tradition, lets create a blogging app.

    $ rails new blog --skip-active-record --skip-bundle -T


Now the first thing to do is add the Ripple gem to the Gemfile. Currently the
latest version doesn't work with latest rails so lets use the latest from git.

    source 'http://rubygems.org'
    gem 'rails', '3.2.2'

    gem 'ripple', '1.0.0.beta'

    group :development, :test do
        gem 'rspec'
        gem 'rspec-rails'
    end

Then install the gems in the usual way.

     $ bundle

Once the gems have installed we'll need to generate the Ripple configuration.

    $ rails g
    ...
    Ripple:
      ripple
      ripple:configuration
      ripple:js
      ripple:model
      ripple:observer
      ripple:test


So we want the default ripple one.

    $ rails g ripple
    create  config/ripple.yml
    create  app/mapreduce
    create  app/mapreduce/contrib.js
    create  app/mapreduce/iso8601.js
    create  app/mapreduce/ripple.js
    insert  spec/spec_helper.rb
    insert  spec/spec_helper.rb


As you can see it creates a ripple configuration file, some mapreduce assets and
updates my spec_helper. Lets look just at the `config/ripple.yml` file for now.

The default config file is shown below. You may have to modify or add the 'source:'
property to point to where you've installed Riak. I've put mine in the same
directory as the rails project so it looks like this

    # TODO use the default homebrew path here.
    # Configure Riak connections for the Ripple library.
    development:
        http_port: 9000
        pb_port: 9001
        host: 127.0.0.1
        source: /usr/local/bin   # Default for Homebrew.

    # The test environment has additional keys for configuring the
    # Riak::TestServer for your test/spec suite:
    #
    # * bin_dir specifies the path to the "riak" script that you use to
    #           start Riak (just the directory)
    # * js_source_dir specifies where your custom Javascript functions for
    #           MapReduce should be loaded from. Usually app/mapreduce.
    test:
        http_port: 9000
        pb_port: 9002
        host: 127.0.0.1
        source: /usr/local/bin   # Default for Homebrew.
        js_source_dir: <%= Rails.root + "app/mapreduce" %>

    production:
        http_port: 8098
        pb_port: 8087
        host: 127.0.0.1
        source: /usr/local/bin   # Default for Homebrew.


With everything in place we can begin to build our application. We'll start by
generating an `Article` model with name and content fields and use the Rails
scaffolding to create all the dependent bits.

    $ rails g scaffold article name:string content:text

Ripple provides generators for models so that Ripple's model generator is
invoked when a model is created and ActiveRecord isn't used. When we open up the
model file we'll see that it's a simple class that includes `Ripple::Document`.

    class Article
        include Ripple::Document
        property :name, String
        property :content, String
    end

The big difference to an ActiveModel model is that the class defines each
property, along with the type.


Ripple also adds a number of helpful rake tasks for Riak.

    $ rake -T
    ...
    rake riak:create        # Creates a Riak cluster for the current environment in db/, e.g. d...
    rake riak:create:all    # Creates Riak clusters for all environments defined in config/ripp...
    rake riak:destroy       # Destroys the generated Riak cluster for the current environment.
    rake riak:drop          # Drops data only from the Riak cluster for the current environment.
    rake riak:drop:all      # Drops data Riak clusters for all environments defined in config/r...
    rake riak:start         # Starts the Riak cluster for the current environment
    rake riak:stop          # Stops the Riak cluster for the current environment

## Load the controller and show some actions
## Make the controller run some basic rspec things
## Show some basic validations on the data.

## Point to Part2
## Running MapReduce queries against Riak
## Pulling in Postgres
## Writing queries in CoffeeScript
## Using the rails console
## Using the Riak Console

## What else does RailsCasts Mongoid cover?
