--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "talks/**/*" $ do
    route idRoute
    compile $ copyFileCompiler

  match (fromList ["about.md", "contact.md"]) $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" allContext
        >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        -- Used by the RSS/Atom feed
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  match "pages/*" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/page.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  -- match "bib/*.md" $ do
  --   route $ setExtension "html"
  --   compile $
  --     pandocCompiler
  --       >>= loadAndApplyTemplate "templates/page.html" postCtx
  --       >>= loadAndApplyTemplate "templates/default.html" postCtx
  --       >>= relativizeUrls

  -- match "bib/*.bib" $ do
  --   route idRoute
  --   compile copyFileCompiler

  -- http://jaspervdj.be/hakyll/tutorials/05-snapshots-feeds.html
  let rss name render' =
        create [name] $ do
          route idRoute
          compile $ do
            let feedCtx = mconcat [bodyField "description", postCtx] -- description must be prepended for some reason
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
            render' feedConfiguration feedCtx posts

  rss "atom.xml" renderAtom
  rss "rss.xml" renderRss

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Archives"
              `mappend` allContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  -- Create HTML redirects for old url structure
  createRedirects
    [ -- TODO Example redirect
      -- ("blog/2009/05/23/acer-aspire-one-with-netbsd-50/index.html", "/posts/2009-05-23-acer-aspire-one-with-netbsd-50.html"),
      ("blog/2007/12/08/postfix-smf/index.html", "/posts/2007-12-08-postfix-smf.html"),
      ("blog/2007/12/14/collecting-failed-ssh-logins/index.html", "/posts/2007-12-14-collecting-failed-ssh-logins.html"),
      ("blog/2007/12/30/programming-erlang/index.html", "/posts/2007-12-30-programming-erlang.html"),
      ("blog/2008/01/01/site-update/index.html", "/posts/2008-01-01-site-update.html"),
      ("blog/2008/01/06/creating-an-rsync-copy-of-nekoware/index.html", "/posts/2008-01-06-creating-an-rsync-copy-of-nekoware.html"),
      ("blog/2008/01/31/minimal-irix-kernel-driver/index.html", "/posts/2008-01-31-minimal-irix-kernel-driver.html"),
      ("blog/2008/02/26/99-erlang-problems-1-15/index.html", "/posts/2008-02-26-99-erlang-problems-1-15.html"),
      ("blog/2008/02/28/hacking-the-meraki/index.html", "/posts/2008-02-28-hacking-the-meraki.html"),
      ("blog/2008/03/01/meraki-internals/index.html", "/posts/2008-03-01-meraki-internals.html"),
      ("blog/2008/03/09/erlang-emacs-fun/index.html", "/posts/2008-03-09-erlang-emacs-fun.html"),
      ("blog/2008/04/07/erlang-techtalk/index.html", "/posts/2008-04-07-erlang-techtalk.html"),
      ("blog/2008/04/10/postfix-smf-update/index.html", "/posts/2008-04-10-postfix-smf-update.html"),
      ("blog/2008/05/07/99-erlang-problems-16-25/index.html", "/posts/2008-05-07-99-erlang-problems-16-25.html"),
      ("blog/2008/05/18/software-engineering-radio-interviews/index.html", "/posts/2008-05-18-software-engineering-radio-interviews.html"),
      ("blog/2008/08/11/posting-from-the-iphone/index.html", "/posts/2008-08-11-posting-from-the-iphone.html"),
      ("blog/2008/09/06/haskell-things/index.html", "/posts/2008-09-06-haskell-things.html"),
      ("blog/2008/09/14/installing-netbsd-on-an-inspiron-8000/index.html", "/posts/2008-09-14-installing-netbsd-on-an-inspiron-8000.html"),
      ("blog/2008/09/20/netbsd-wireless-setup/index.html", "/posts/2008-09-20-netbsd-wireless-setup.html"),
      ("blog/2008/10/06/quick-os-x-tip/index.html", "/posts/2008-10-06-quick-os-x-tip.html"),
      ("blog/2008/10/12/simple-tcp-server-in-erlang/index.html", "/posts/2008-10-12-simple-tcp-server-in-erlang.html"),
      ("blog/2008/12/14/update-on-me/index.html", "/posts/2008-12-14-update-on-me.html"),
      ("blog/2008/12/26/reading-list-for-the-holidays/index.html", "/posts/2008-12-26-reading-list-for-the-holidays.html"),
      ("blog/2008/12/27/start-a-side-project/index.html", "/posts/2008-12-27-start-a-side-project.html"),
      ("blog/2008/12/29/streaming-itunes-from-solaris-10/index.html", "/posts/2008-12-29-streaming-itunes-from-solaris-10.html"),
      ("blog/2009/01/25/lambdacats/index.html", "/posts/2009-01-25-lambdacats.html"),
      ("blog/2009/03/05/compiler-update/index.html", "/posts/2009-03-05-compiler-update.html"),
      ("blog/2009/03/21/yarb-yet-another-ring-benchmark-part-i/index.html", "/posts/2009-03-21-yarb-yet-another-ring-benchmark-part-i.html"),
      ("blog/2009/05/23/acer-aspire-one-with-netbsd-50/index.html", "/post/2009-05-23-acer-aspire-one-with-netbsd-50.html"),
      ("blog/2009/05/30/xmonad-install-on-netbsd-50/index.html", "/posts/2009-05-30-xmonad-install-on-netbsd-50.html"),
      ("blog/2009/06/30/update/index.html", "/posts/2009-06-30-update.html"),
      ("blog/2009/09/13/forcing-snow-leopard-to-use-64bit-kernel/index.html", "/posts/2009-09-13-forcing-snow-leopard-to-use-64bit-kernel.html"),
      ("blog/2010/03/23/netbsd-5-0-on-an-aspireone-110l/index.html", "/posts/2010-03-23-netbsd-5-0-on-an-aspireone-110l.html"),
      ("blog/2010/05/05/ebook-thoughts/index.html", "/posts/2010-05-05-ebook-thoughts.html"),
      ("blog/2010/06/25/declaring-.emacs-bankruptcy/index.html", "/posts/2010-06-25-declaring-.emacs-bankruptcy.html"),
      ("blog/2010/06/25/emacs-publishing/index.html", "/posts/2010-06-25-emacs-publishing.html"),
      ("blog/2010/08/24/thoughts-on-objectivec-and-cocoa/index.html", "/posts/2010-08-24-thoughts-on-objectivec-and-cocoa.html"),
      ("blog/2010/09/30/leaving-openbsd-for/index.html", "/posts/2010-09-30-leaving-openbsd-for.html"),
      ("blog/2010/12/17/intellij-idea-ubuntu-launcher/index.html", "/posts/2010-12-17-intellij-idea-ubuntu-launcher.html"),
      ("blog/2011/03/08/ubuntu-server-update/index.html", "/posts/2011-03-08-ubuntu-server-update.html"),
      ("blog/2011/06/24/rails-ora-000972-and-you/index.html", "/posts/2011-06-24-rails-ora-000972-and-you.html"),
      ("blog/2011/07/05/the-rubinius-virtual-machine/index.html", "/posts/2011-07-05-the-rubinius-virtual-machine.html"),
      ("blog/2011/07/25/setting-up-oracle-10g-xe-with-rails-3/index.html", "/posts/2011-07-25-setting-up-oracle-10g-xe-with-rails-3.html"),
      ("blog/2011/08/12/emacs-reboot/index.html", "/posts/2011-08-12-emacs-reboot.html"),
      ("blog/2011/10/05/agressive-emacs-spring-clean/index.html", "/posts/2011-10-05-agressive-emacs-spring-clean.html"),
      ("blog/2011/10/06/ipad-and-ebooks-galore/index.html", "/posts/2011-10-06-ipad-and-ebooks-galore.html"),
      ("blog/2011/10/08/git-and-mercurial-workflow-gist/index.html", "/posts/2011-10-08-git-and-mercurial-workflow-gist.html"),
      ("blog/2012/01/22/site-update/index.html", "/posts/2012-01-22-site-update.html"),
      ("blog/2012/02/01/thoughts-on-gnu-hurd/index.html", "/posts/2012-02-01-thoughts-on-gnu-hurd.html"),
      ("blog/2012/02/10/git-rebase-workflow/index.html", "/posts/2012-02-10-git-rebase-workflow.html"),
      ("blog/2012/03/15/riak-ruby-ripple-talk/index.html", "/posts/2012-03-15-riak-ruby-ripple-talk.html"),
      ("blog/2012/04/12/webmachine-talk/index.html", "/posts/2012-04-12-webmachine-talk.html"),
      ("blog/2012/09/10/sinan-intro/index.html", "/posts/2012-09-10-sinan-intro.html"),
      ("blog/2013/09/02/erlang-hot-code-loading/index.html", "/posts/2013-09-02-erlang-hot-code-loading.html"),
      ("blog/2013/09/17/emberjs-and-google-analytics/index.html", "/posts/2013-09-17-emberjs-and-google-analytics.html"),
      ("blog/2014/08/08/lambdajam-talk/index.html", "/posts/2014-08-08-lambdajam-talk.html"),
      ("blog/2015/01/16/ocaml-lenses/index.html", "/posts/2015-01-16-ocaml-lenses.html"),
      ("blog/2015/05/15/unreliable-guide-to-ocaml-modules/index.html", "/posts/2015-05-15-unreliable-guide-to-ocaml-modules.html"),
      ("blog/2015/05/20/first-month-of-haskell/index.html", "/posts/2015-05-20-first-month-of-haskell.html"),
      ("blog/2015/08/17/ocaml-ffi-bindings/index.html", "/posts/2015-08-17-ocaml-ffi-bindings.html"),
      ("blog/2018/06/22/transformers-either/index.html", "/posts/2018-06-22-transformers-either.html")
    ]

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Home"
              `mappend` allContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  -- TODO Create a sitemap.xml
  -- create ["sitemap.xml"] $ do
  --   route idRoute
  --   compile $ do
  --     -- load and sort the posts
  --     posts <- recentFirst =<< loadAll "posts/*"

  --     -- load individual pages from a list (globs DO NOT work here)
  --     singlePages <- loadAll (fromList ["about.rst", "contact.markdown"])

  --     -- mappend the posts and singlePages together
  --     let pages = posts <> singlePages

  --         -- create the `pages` field with the postCtx
  --         -- and return the `pages` value for it
  --         sitemapCtx = listField "pages" postCtx (return pages)

  --     -- make the item and apply our sitemap template
  --     makeItem ""
  --       >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

  match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  allContext

allContext =
  field "siteTitle" (\_ -> return "Tim McGilchrist")
    `mappend` field "baseurl" (\_ -> return "")
    `mappend` dateField "date" "%B %e, %Y"
    `mappend` defaultContext

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle = "Perpetually Curious Blog",
      feedDescription =
        "Personal opinions on technology,\
        \functional programming and various systems topics.",
      feedAuthorName = "Tim McGilchrist",
      feedAuthorEmail = "timmcgil@gmail.com",
      feedRoot = "https://lambdafoo.com"
    }
