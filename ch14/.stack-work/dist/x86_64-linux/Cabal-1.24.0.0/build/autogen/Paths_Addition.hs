{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Addition (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/kai/Projects/haskellbook/ch14/.stack-work/install/x86_64-linux/lts-7.0/8.0.1/bin"
libdir     = "/home/kai/Projects/haskellbook/ch14/.stack-work/install/x86_64-linux/lts-7.0/8.0.1/lib/x86_64-linux-ghc-8.0.1/Addition-0.1.0.0-44tsVVIga5OB1GDSHJIz9G"
datadir    = "/home/kai/Projects/haskellbook/ch14/.stack-work/install/x86_64-linux/lts-7.0/8.0.1/share/x86_64-linux-ghc-8.0.1/Addition-0.1.0.0"
libexecdir = "/home/kai/Projects/haskellbook/ch14/.stack-work/install/x86_64-linux/lts-7.0/8.0.1/libexec"
sysconfdir = "/home/kai/Projects/haskellbook/ch14/.stack-work/install/x86_64-linux/lts-7.0/8.0.1/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Addition_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Addition_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Addition_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Addition_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Addition_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
