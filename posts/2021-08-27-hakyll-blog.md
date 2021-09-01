---
title: Hakyll Blog setup
author: Tim McGilchrist
date: 2021-08-27 00:00
tags: haskell
description: How I setup my blog with Hakyll
---

I wanted to port my blog across from an old Jeykll setup to Haykll.
The Jekyll was out of date and keeping the required ruby tools installed
when I swapped machines was a huge pain. I don't write ruby much anymore.

Considering my options, I looked at Hugo and Hakyll, discarding Hugo because
I don't want to keep up with the JS churn, even though they have lots of
great resources and themes available. So Hakyll seems like the best option. I
already regularily write Haskell so the tools will be up to date and I can make
it do everything I want by digging into the source code.

My requirements are:

 * Markdown based workflow
 * support basic pages
 * individual post with code highlighting
 * RSS/Atom feed
 * GitHub action based build and deploy
 * support old blog URLs (HTML URL redirects to new url structure)
 * serve js talks/slides directly from Hakyll
 * generated sitemap.xml
 * integrate Google Analytics

Getting Hakyll Setup
----------

First things first! I like the following layout when setting up a basic Haskell project:

```
$ tree -L 1
.
├── CNAME
├── LICENSE
├── README.md
├── css
├── drafts
├── images
├── index.html
├── lambdafoo.cabal
├── main
├── pages
├── posts
├── talks
└── templates
```

Initially I used `cabal init --cabal-version=2.4 --license=BSD3 -p lambdafoo.com` to get a skeleton
project with a reasonable cabal file. Then I moved things around, making `main/site.hs` the
entry point for running Hakyll and adding a TODO list of features into the README.md

``` yaml
 * ~~basic pages~~
   * ~~about~~
   * ~~talks~~
   * ~~archive~~
 * ~~individual post with code highlighting~~
 * ~~rss/atom feed~~
 * ~~add rss/atom feed to archive page~~
 * ~~github action build and deploy~~
 * ~~html url redirects to new url structure~~
 * ~~serve js talks/slides directly from Hakyll~~
 * configure dependabot for Haskell
 * ~~add generated sitemap.xml~~
 * ~~integrate Google Analytics~~
```

These directories are used for Hakyll content:

 * pages - includes various regular pages on the site like talks or about me
 * css - includes the style sheets for the HTML
 * images - is the static images for the site
 * drafts - containts the draft posts I'm writing
 * talks - contains static JS/HTML based slides from presentations that I want to serve directly
           from the site
 * templates - site templates in a markup language for doing page layouts
 * CNAME - is Github Pages hosting to tell it the DNS name for the site

The trickiest part was getting a version of the cabal file that worked with GHC 8.10 and a
recent version of Hakyll. I ended up needing to pin Hakyll as `hakyll ^>= 4.13` and left the
other dependencies floating.

``` yaml
executable site
  main-is:             site.hs
  hs-source-dirs:      main
  default-language:    Haskell2010
  build-depends:
                       base      >= 4.6  && < 5
                     , binary    >= 0.5
                     , directory >= 1.2
                     , filepath  >= 1.3
                     , hakyll    ^>= 4.13
                     , blaze-html
                     , lens
                     , time
                     , aeson
                     , lens-aeson
                     , containers
                     , pandoc
                     , process   >= 1.6
                     , text      >= 1.2

```

At this point, I could have either continued setting up Hakyll or setup CI. I usually
prefer setting up CI as early as possible in a project, so I stared there.
Here is what that looks like:

Hakyll CI
----------

There are a few options for cloud CI, and my requirements were simple: no cost, easy setup, and integration
with GitHub pages where I host my site. It was a toss up between CircleCI and Github Actions, as I've had
good experience with CircleCI, but Idecided to try Github Actions.

First, create a directory `mkdir -p .github/workflows/` with a `ci.yml` file

``` yaml
name: CI
on:
  push:
    branches:
      - master
  pull_request:
    types:
      - opened
      - synchronize
jobs:
  build:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        cabal: ["3.4.0.0"]
        ghc: ["8.10.7"]

```

The `matrix` section sets up a build for `ghc 8.10.7` and cabal `3.4`, which is enough for a simple blog,
but is where you'd add extra options, for say a library. Next, we use some community GitHub Actions to
`checkout` and setup Haskell.

``` yaml

    steps:
      - uses: actions/checkout@v2
      - uses: haskell/actions/setup@v1
        id: setup-haskell-cabal
        with:
          ghc-version: ${{ matrix.ghc }}
          cabal-version: ${{ matrix.cabal }}

```

Here we run `cabal update` to update our Hackage index and then setup some build caching for
our dependencies. You can copy this directly and it should work:

``` yaml

      - name: Cabal Update
        run: |
          cabal v2-update
          cabal v2-freeze $CONFIG
      - uses: actions/cache@v2.1.4
        with:
          path: |
            ${{ steps.setup-haskell-cabal.outputs.cabal-store }}
            dist-newstyle
          key: ${{ runner.os }}-${{ matrix.ghc }}-${{ hashFiles('cabal.project.freeze') }}
          restore-keys: |
            ${{ runner.os }}-${{ matrix.ghc }}-

```

Then we run the cabal build and Hakyll site build.

``` yaml

      - name: Build Site
        run: |
          cabal v2-build $CONFIG
          cabal exec site build
```

Adding that into your repo's  main branch of your repo should yield a working CI.
On top of that, I added a dependabot configuration to check that my GitHub Actions
config was up to date.

Add a file `dependabot.yml` to `.github`:

``` yaml
version: 2
updates:
  - package-ecosystem: "github-actions"
    directory: "/"
    schedule:
      interval: "daily"
    commit-message:
      prefix: "GA"
      include: "scope"
    labels:
      - "CI"
```
This will check that your GitHub Actions use the latest version and open a PR to bump versions
if you aren't. Something like this for Haskell would be super sweet.

Generating the Site
----------

Let's quickly walk through the contents of `main/site.hs`, but there are more in-depth tutorials on the
main [Hakyll site](https://jaspervdj.be/hakyll/tutorials.html)

``` haskell
{-# LANGUAGE OverloadedStrings #-}

import Hakyll

main :: IO ()
main = hakyll $ do
```

Here we import Hakyll, setup overloaded strings, and create a main function:

``` haskell
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler
```

Serve stylesheets and images from directories `css` and `images`, respectively. This is standard code
that can be copied directly, it basically copies the files into the final static site directory `_site`.

Next I wanted to serve some old talk slides written in HTML and JavaScript directly from my site.
I couldn't find any posts talking about how to do this, but after thinking about it, I realized that I
just wanted to serve static assets again like the `css` and `images` above. So that's exactly what was required!
If course, I lie. I had to fix a few hard coded paths in the HTML but otherwise it worked.

The layout for `talks` looks like:

```
talks
├── erl-syd-2012-webmachine
├── fp-syd-freer-2016
├── fp-syd-higher-2015
├── lambda-jam-2014-raft
├── lambda-jam-2015-ocaml-functors
├── lambda-jam-2016-performance
├── roro-2012-riak
└── scala-syd-2015-modules
```

So I needed an extra wildcard in my `match` statement:

``` haskell
  match "talks/**/*" $ do
    route idRoute
    compile $ copyFileCompiler
```

This content then gets served under `lambdafoo.com/talks/scala-syd-2015-modules/`.
In retrospect, this is an obvious solution to serving any static content generated outside of Hakyll,
but it did take me a while to realise it.

Next we load the individual blog posts:

``` haskell
  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        -- Used by the RSS/Atom feed
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

```

Authoring Posts
----------
After getting a few simple things out of the way, the Markdown-based workflow already worked with
Hakyll, so there's nothing really to see there. Creating a simple YAML file with the following meta-data
and content is enough to get a simple post working.

``` yaml
---
title: Hakyll Blog setup
author: Tim McGilchrist
date: 2021-02-01 00:00
tags: haskell
description: How I setup my blog with Hakyll
---

Content of post
```

Deploying
----------

I have a domain `lambdafoo.com` that I use to serve my blog. Github pages has up-to-date
information on how to set this up with your DNS provider.

Here is where choosing Github Actions really pays off! There is a community action to do it all!
Assuming you've turned on GitHub Pages in the settings for you repo, add this to the end of the `ci.yml`:

``` yml
      - name: Deploy 🚀
        uses: JamesIves/github-pages-deploy-action@4.1.5
        if: github.ref == 'refs/heads/master'
        with:
          token: ${{ secrets.GITHUB_TOKEN }}
          branch: gh-pages # The branch the action should deploy to.
          folder: _site # The folder the action should deploy.
          clean: true # Automatically remove deleted files from the deploy branch

```

This deploys the output of the `Build Site` step from folder `_site` to the branch `gh-pages`
on all `master` builds (controlled via `if: github.ref == 'refs/heads/master'`).

On the first build, there is a bit of lag to deploy. I had issues with my DNS setup and two
personal repositories using the same CNAME values. Apart from that, the process was smooth, and I
quickly had a new version working. Again, if you setup dependabot, it will check that this action is
up-to-date.

Resources
----------

 * [Blog source](https://github.com/tmcgilchrist/lambdafoo.com)
 * [GitHub Actions for Haskell](https://markkarpov.com/post/github-actions-for-haskell-ci.html)
 * [Dependabot](https://kodimensional.dev/github-actions#dependabot)
