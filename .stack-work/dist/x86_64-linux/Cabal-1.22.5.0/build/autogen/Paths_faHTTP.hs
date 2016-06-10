module Paths_faHTTP (
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

bindir     = "/home/shubham/FocusAnalytics/Haskell/faHTTP/.stack-work/install/x86_64-linux/lts-6.2/7.10.3/bin"
libdir     = "/home/shubham/FocusAnalytics/Haskell/faHTTP/.stack-work/install/x86_64-linux/lts-6.2/7.10.3/lib/x86_64-linux-ghc-7.10.3/faHTTP-0.1.0.0-4gXpjLx51zk3ilhR7qQ1zn"
datadir    = "/home/shubham/FocusAnalytics/Haskell/faHTTP/.stack-work/install/x86_64-linux/lts-6.2/7.10.3/share/x86_64-linux-ghc-7.10.3/faHTTP-0.1.0.0"
libexecdir = "/home/shubham/FocusAnalytics/Haskell/faHTTP/.stack-work/install/x86_64-linux/lts-6.2/7.10.3/libexec"
sysconfdir = "/home/shubham/FocusAnalytics/Haskell/faHTTP/.stack-work/install/x86_64-linux/lts-6.2/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "faHTTP_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "faHTTP_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "faHTTP_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "faHTTP_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "faHTTP_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
