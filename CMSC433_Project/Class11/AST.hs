{- 
---
fulltitle: "In class exercise: AST parsing and printing"
date: March 4, 2025
---

-}

module AST where

import Control.Applicative (Alternative(..))
import System.IO
import Prelude hiding (filter)
import Data.Char (isDigit, isAlpha )


import Text.PrettyPrint ( (<+>), Doc )
import qualified Text.PrettyPrint as PP

import ParserCombinators (Parser, doParse, satisfy, char, string, filter)

data Exp = Num Int
         | Var String
         | Plus Exp Exp
         | Mult Exp Exp
         deriving (Eq, Show)

-- Pretty Print

class PP a where
    pp :: a -> Doc

instance PP String where 
    pp = PP.text

instance PP Int where
    pp = PP.int

instance PP Exp where 
    pp (Num n) = pp n
    pp (Var s) = pp s
    pp (Plus e1 e2) = paren (e1) <+> PP.char '+' <+> paren (e2)
    pp (Mult e1 e2) = paren (e1) <+> PP.char '*' <+> paren (e2)

paren :: Exp -> Doc
paren e | isBase e = pp e
paren e = PP.parens (pp e)

isBase :: Exp -> Bool
isBase (Num _) = True
isBase (Var _) = True
isBase _ = False

-- PP.render $ pp (Plus (Num 12) (Mult (Num 34) (Num 12)))
--"12 + (34 * 12)"

-- Borrowed from lecture

alphaChar, digitChar :: Parser Char
alphaChar = satisfy isAlpha
digitChar = satisfy isDigit

oneNat :: Parser Int
oneNat = read <$> (some digitChar)   -- know that read will succeed because input is all digits

-- Numbers
numP :: Parser Exp
numP = Num <$> oneNat

-- Variables (TODO: first alpha, then alpha or digit)
varP :: Parser Exp
varP = Var <$> (some alphaChar)


expP :: Parser Exp
expP = addP <|> multP where
    addP = (\e1 _ e2 -> Plus e1 e2) <$> (multP <|> leafP) <*> char '+' <*> (addP <|> multP <|> leafP) where
    multP = (\e1 _ e2 -> Mult e1 e2) <$> leafP <*> char '*' <*> (multP <|> leafP) where
    leafP = numP <|> varP

-- fmap and applicative: 
-- addP = (Exp -> Char -> Exp -> Exp) <$> Parser Exp <*> Parser Char <*> Parser Exp
--      = Parser (Char -> Exp -> Exp) <*> Parser Char <*> Parser Exp
--      = Parser (Exp -> Exp) <*> Parser Exp
--      = Parser Exp