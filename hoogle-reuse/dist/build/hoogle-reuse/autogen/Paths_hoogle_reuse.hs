{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_hoogle_reuse (
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
version = Version [1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/yuta/.cabal/bin"
libdir     = "/home/yuta/.cabal/lib/x86_64-linux-ghc-7.10.3/hoogle-reuse-1.0.0-EWjZU7OIPXVLcUj9kbFukQ"
dynlibdir  = "/home/yuta/.cabal/lib/x86_64-linux-ghc-7.10.3"
datadir    = "/home/yuta/.cabal/share/x86_64-linux-ghc-7.10.3/hoogle-reuse-1.0.0"
libexecdir = "/home/yuta/.cabal/libexec/x86_64-linux-ghc-7.10.3/hoogle-reuse-1.0.0"
sysconfdir = "/home/yuta/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hoogle_reuse_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hoogle_reuse_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hoogle_reuse_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hoogle_reuse_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hoogle_reuse_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hoogle_reuse_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
