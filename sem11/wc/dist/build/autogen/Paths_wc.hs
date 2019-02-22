module Paths_wc (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/maria/.cabal/bin"
libdir     = "/home/maria/.cabal/lib/x86_64-linux-ghc-7.10.3/wc-0.1.0.0-6J46K1n797hEXt4nCZfhFj"
datadir    = "/home/maria/.cabal/share/x86_64-linux-ghc-7.10.3/wc-0.1.0.0"
libexecdir = "/home/maria/.cabal/libexec"
sysconfdir = "/home/maria/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "wc_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "wc_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "wc_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "wc_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "wc_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
