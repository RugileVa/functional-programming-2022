{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_bitvec (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [1,1,3,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\cabal\\store\\ghc-8.10.7\\bitvec-1.1.3.0-59473e415ab95b1fcf0367b1ef7d94622e06137c\\bin"
libdir     = "C:\\cabal\\store\\ghc-8.10.7\\bitvec-1.1.3.0-59473e415ab95b1fcf0367b1ef7d94622e06137c\\lib"
dynlibdir  = "C:\\cabal\\store\\ghc-8.10.7\\bitvec-1.1.3.0-59473e415ab95b1fcf0367b1ef7d94622e06137c\\lib"
datadir    = "C:\\cabal\\store\\ghc-8.10.7\\bitvec-1.1.3.0-59473e415ab95b1fcf0367b1ef7d94622e06137c\\share"
libexecdir = "C:\\cabal\\store\\ghc-8.10.7\\bitvec-1.1.3.0-59473e415ab95b1fcf0367b1ef7d94622e06137c\\libexec"
sysconfdir = "C:\\cabal\\store\\ghc-8.10.7\\bitvec-1.1.3.0-59473e415ab95b1fcf0367b1ef7d94622e06137c\\etc"

getBinDir     = catchIO (getEnv "bitvec_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "bitvec_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "bitvec_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "bitvec_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bitvec_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "bitvec_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
