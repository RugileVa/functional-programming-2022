{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_RSA (
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
version = Version [2,4,1] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\cabal\\store\\ghc-8.10.7\\RSA-2.4.1-504eb1cafd005af297f0f826ab5fe5a113de6635\\bin"
libdir     = "C:\\cabal\\store\\ghc-8.10.7\\RSA-2.4.1-504eb1cafd005af297f0f826ab5fe5a113de6635\\lib"
dynlibdir  = "C:\\cabal\\store\\ghc-8.10.7\\RSA-2.4.1-504eb1cafd005af297f0f826ab5fe5a113de6635\\lib"
datadir    = "C:\\cabal\\store\\ghc-8.10.7\\RSA-2.4.1-504eb1cafd005af297f0f826ab5fe5a113de6635\\share"
libexecdir = "C:\\cabal\\store\\ghc-8.10.7\\RSA-2.4.1-504eb1cafd005af297f0f826ab5fe5a113de6635\\libexec"
sysconfdir = "C:\\cabal\\store\\ghc-8.10.7\\RSA-2.4.1-504eb1cafd005af297f0f826ab5fe5a113de6635\\etc"

getBinDir     = catchIO (getEnv "RSA_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "RSA_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "RSA_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "RSA_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "RSA_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "RSA_sysconfdir") (\_ -> return sysconfdir)




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
