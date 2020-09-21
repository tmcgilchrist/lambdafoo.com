{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_oni_tech (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/tim/.cabal/bin"
libdir     = "/Users/tim/.cabal/lib/x86_64-osx-ghc-8.4.4/oni-tech-0.1.0-inplace-site"
dynlibdir  = "/Users/tim/.cabal/lib/x86_64-osx-ghc-8.4.4"
datadir    = "/Users/tim/.cabal/share/x86_64-osx-ghc-8.4.4/oni-tech-0.1.0"
libexecdir = "/Users/tim/.cabal/libexec/x86_64-osx-ghc-8.4.4/oni-tech-0.1.0"
sysconfdir = "/Users/tim/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "oni_tech_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "oni_tech_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "oni_tech_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "oni_tech_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "oni_tech_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "oni_tech_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
