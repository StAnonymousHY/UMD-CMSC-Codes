{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_haskell_intro (
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
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\Users\\YHXHa\\Desktop\\UMD-CMSC-Codes\\CMSC433_Project\\Project02_HaskellBasics\\.stack-work\\install\\d3b0f0cc\\bin"
libdir     = "C:\\Users\\YHXHa\\Desktop\\UMD-CMSC-Codes\\CMSC433_Project\\Project02_HaskellBasics\\.stack-work\\install\\d3b0f0cc\\lib\\x86_64-windows-ghc-9.4.5\\haskell-intro-0.1.0.0-LE8q5Mr4Dn8HwPrA9myE1B-hw01"
dynlibdir  = "C:\\Users\\YHXHa\\Desktop\\UMD-CMSC-Codes\\CMSC433_Project\\Project02_HaskellBasics\\.stack-work\\install\\d3b0f0cc\\lib\\x86_64-windows-ghc-9.4.5"
datadir    = "C:\\Users\\YHXHa\\Desktop\\UMD-CMSC-Codes\\CMSC433_Project\\Project02_HaskellBasics\\.stack-work\\install\\d3b0f0cc\\share\\x86_64-windows-ghc-9.4.5\\haskell-intro-0.1.0.0"
libexecdir = "C:\\Users\\YHXHa\\Desktop\\UMD-CMSC-Codes\\CMSC433_Project\\Project02_HaskellBasics\\.stack-work\\install\\d3b0f0cc\\libexec\\x86_64-windows-ghc-9.4.5\\haskell-intro-0.1.0.0"
sysconfdir = "C:\\Users\\YHXHa\\Desktop\\UMD-CMSC-Codes\\CMSC433_Project\\Project02_HaskellBasics\\.stack-work\\install\\d3b0f0cc\\etc"

getBinDir     = catchIO (getEnv "haskell_intro_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "haskell_intro_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "haskell_intro_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "haskell_intro_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_intro_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_intro_sysconfdir") (\_ -> return sysconfdir)




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
