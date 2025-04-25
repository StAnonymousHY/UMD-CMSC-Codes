module Main where

-- Make sure you run this via `stack run` in the top-level folder.

import Syntax
import Printer
import qualified ParserLib as P
import Parser

import Control.Monad
import System.Environment
import Test.QuickCheck

main :: IO ()
main = do
  a <- getArgs
  s <- parseDafnyFile (head a)
  case s of
    Left err -> error err -- Giving better error messages is hard!
    Right p -> do
      putStrLn "Parsed Method:"
      putStrLn "=============="      
      putStrLn $ show p
      putStrLn ""
      putStrLn "Pretty Printed:"
      putStrLn "==============="            
      putStrLn $ pretty p
