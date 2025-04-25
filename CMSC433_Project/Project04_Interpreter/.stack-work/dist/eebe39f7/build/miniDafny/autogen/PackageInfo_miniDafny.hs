{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_miniDafny (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "miniDafny"
version :: Version
version = Version [1,0,0] []

synopsis :: String
synopsis = "Project Development for CSMC 433 "
copyright :: String
copyright = ""
homepage :: String
homepage = "https://www.cs.umd.edu/class/spring2023/cmsc433/"
