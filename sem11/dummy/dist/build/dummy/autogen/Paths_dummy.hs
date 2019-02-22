{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_dummy (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/edgar/.cabal/bin"
libdir     = "/home/edgar/.cabal/lib/x86_64-linux-ghc-8.4.3/dummy-0.1.0.0-7IFdyvwTzI63wRhPXkTpNU-dummy"
dynlibdir  = "/home/edgar/.cabal/lib/x86_64-linux-ghc-8.4.3"
datadir    = "/home/edgar/.cabal/share/x86_64-linux-ghc-8.4.3/dummy-0.1.0.0"
libexecdir = "/home/edgar/.cabal/libexec/x86_64-linux-ghc-8.4.3/dummy-0.1.0.0"
sysconfdir = "/home/edgar/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "dummy_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "dummy_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "dummy_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "dummy_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "dummy_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "dummy_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
