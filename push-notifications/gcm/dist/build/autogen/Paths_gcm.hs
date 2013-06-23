module Paths_gcm (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/marcos/.cabal/bin"
libdir     = "/home/marcos/.cabal/lib/gcm-0.1.0.0/ghc-7.4.2"
datadir    = "/home/marcos/.cabal/share/gcm-0.1.0.0"
libexecdir = "/home/marcos/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "gcm_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "gcm_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "gcm_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "gcm_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
