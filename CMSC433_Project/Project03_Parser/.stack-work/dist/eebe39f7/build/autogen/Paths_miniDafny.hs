{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_miniDafny (
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
version = Version [1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\Users\\YHXHa\\Desktop\\UMD-CMSC-Codes\\CMSC433_Project\\Project03_Parser\\.stack-work\\install\\77d2fa40\\bin"
libdir     = "C:\\Users\\YHXHa\\Desktop\\UMD-CMSC-Codes\\CMSC433_Project\\Project03_Parser\\.stack-work\\install\\77d2fa40\\lib\\x86_64-windows-ghc-9.6.5\\miniDafny-1.0.0-1EMiIflhn9NFEhHVX1biC5"
dynlibdir  = "C:\\Users\\YHXHa\\Desktop\\UMD-CMSC-Codes\\CMSC433_Project\\Project03_Parser\\.stack-work\\install\\77d2fa40\\lib\\x86_64-windows-ghc-9.6.5"
datadir    = "C:\\Users\\YHXHa\\Desktop\\UMD-CMSC-Codes\\CMSC433_Project\\Project03_Parser\\.stack-work\\install\\77d2fa40\\share\\x86_64-windows-ghc-9.6.5\\miniDafny-1.0.0"
libexecdir = "C:\\Users\\YHXHa\\Desktop\\UMD-CMSC-Codes\\CMSC433_Project\\Project03_Parser\\.stack-work\\install\\77d2fa40\\libexec\\x86_64-windows-ghc-9.6.5\\miniDafny-1.0.0"
sysconfdir = "C:\\Users\\YHXHa\\Desktop\\UMD-CMSC-Codes\\CMSC433_Project\\Project03_Parser\\.stack-work\\install\\77d2fa40\\etc"

getBinDir     = catchIO (getEnv "miniDafny_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "miniDafny_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "miniDafny_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "miniDafny_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "miniDafny_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "miniDafny_sysconfdir") (\_ -> return sysconfdir)



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
