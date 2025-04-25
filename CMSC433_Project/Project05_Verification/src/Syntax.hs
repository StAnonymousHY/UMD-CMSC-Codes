{- | Mini Dafny - Syntax |
   -----------------------

This module defines data structures to represent the syntax of the "miniDafny" programming language.
You should read this file carefully to understand the miniDafny language, but you do not need to
edit this file.

This module contains:

1. The definitions of the datatypes that represent the abstract syntax of miniDafny
2. Sample programs written in miniDafny

-}

module Syntax where

import Data.List(intersperse)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad(mapM_)
import qualified Data.Char as Char

import Test.HUnit

{- |
What is a miniDafny Program?
=====================

The general idea is that miniDafny is a very, very cut down version of the Dafny
verification language. 

A program is a single Method with a name, a list of input variables,
a list of output variables, a sequence of requires/ensures/modifies
statements, followed by a main body.
-}

data Method = Method Name [Binding] [Binding] [Specification] Block
  deriving (Eq, Show)

-- | A Name is just a type synonym for a String:

type Name = String            -- either the name of a variable or the name of a method

-- | A Binding is a Name together with its Type:

type Binding = (Name, Type)

-- | For simplicity, types in miniDafny can be integers, booleans, or arrays of integers.

data Type = TInt | TBool | TArrayInt
  deriving (Eq, Ord, Show)

-- | Specifications are logical statements that describe program behaviors.
-- | They can be requires, ensures or modifies statements.

data Specification =
    Requires Predicate
  | Ensures  Predicate
  deriving (Eq, Show)

-- | A Predicate is a forall-quantified boolean formula, potentially with a precondition:
data Predicate =
    Forall [Binding] Expression
  | PredOp Predicate Bop Predicate
  deriving (Eq, Show)

-- | Programs are sequences of statements in a block:

newtype Block = Block [ Statement ]                 -- s1 ... sn 
  deriving (Eq, Show)

-- | For convenience, we create these instances to join blocks together:

instance Semigroup Block where
   (<>) :: Block -> Block -> Block
   Block s1 <> Block s2 = Block (s1 <> s2)
instance Monoid Block where
   mempty :: Block
   mempty = Block []

-- | Statements themselves have the following forms:
data Statement =
    Decl Binding Expression            -- var x : int := e;
  | Assign Var Expression              -- x := e;
  | If Expression Block Block          -- if e { s1 } else { s2 } 
  | While Predicate Expression Block   -- while e invariant p { s }
  deriving (Eq, Show)

-- | Expressions are variables, literal constants, or operators applied
-- | to sub-expressions:

data Expression =
    Var Var                            -- global variables x and array indexing
  | Val Value                          -- literal values
  | Op1 Uop Expression                 -- unary operators
  | Op2 Expression Bop Expression      -- binary operators
  deriving (Eq, Ord, Show)

{- | The literal values include ints, booleans, and a special value for
     arrays that should not appear directly in source programs, but is
     used by the interpreter.
-}

data Value =
    IntVal Int         -- 1
  | BoolVal Bool       -- false, true
  | ArrayVal [Int]
  deriving (Eq, Show, Ord)

-- | Unary operators are single argument functions: arithmetic negation, logical not, and a
-- | length operation for arrays.

data Uop =
    Neg   -- `-` :: Int -> Int
  | Not   -- `!` :: a -> Bool
  | Len   -- `.Length` :: Table -> Int
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Binary operators are two-argument functions: arithmetic and comparison operators for
-- | integer values, and boolean connectives for boolean values.

data Bop =
    Plus     -- `+`  :: Int -> Int -> Int
  | Minus    -- `-`  :: Int -> Int -> Int
  | Times    -- `*`  :: Int -> Int -> Int
  | Divide   -- `/`  :: Int -> Int -> Int   -- floor division
  | Modulo   -- `%`  :: Int -> Int -> Int   -- modulo
  | Eq       -- `==` :: Int -> Int -> Bool
  | Neq      -- `!=` :: Int -> Int -> Bool> 
  | Gt       -- `>`  :: Int -> Int -> Bool
  | Ge       -- `>=` :: Int -> Int -> Bool
  | Lt       -- `<`  :: Int -> Int -> Bool
  | Le       -- `<=` :: Int -> Int -> Bool
  | Conj     -- `&&` :: Bool -> Bool -> Bool
  | Disj     -- `||` :: Bool -> Bool -> Bool
  | Implies  -- `==>` :: Bool -> Bool -> Bool
  | Iff      -- `<==>` :: Bool -> Bool -> Bool
  deriving (Eq, Ord, Show, Enum, Bounded)

{- | Variables and Arrays |
   ------------------------

Variables are places that store values. 

Arrays of integers are a primitive part of the language. They are data
structure that can be accessed by looking up the value associated
with an integer key, in a variable expression

    t[1]    -- any integer value can be used as a key

or modified by introducing a new value associated with a key, using an
assignment statement:

    t[1] = 3      -- any integer value can be stored in a table

We represent these globals and table fields, using the
following datatype definitions.

-}

data Var =
    Name Name            -- x, global variable
  | Proj Name Expression -- a[1], access array table using an integer
  deriving (Eq, Ord, Show)

{- | Test Programs |
   =================

Below are some test programs that you can use in this assignment. These programs can also be
found in the corresponding files in the `dafny` folder. Please take a look at these files to 
familiarize yourself with the concrete syntax of MiniDafny. A few of them have been indented
to make their structure clear for your convenience; the rest exist for you to use as tests
if you want.

-}

wAbs =
  Method "Abs"
    [("r",TInt)]
    [("absR",TInt)]
    []
    (Block [If (Op2 (Var (Name "r")) Lt (Val (IntVal 0)))
               (Block [Assign (Name "absR") (Op1 Neg (Var (Name "r")))])
               (Block [Assign (Name "absR") (Var (Name "r"))])])

wLoopToZero =
  Method "LoopToZero"
         [("m",TInt),("p",TInt)]
         [("x",TInt),("z",TInt)] 
         [ Requires (Forall [] (Op2 (Var (Name "m")) Gt (Val (IntVal 0))))
         , Ensures (Forall [] (Op2 (Var (Name "z")) Eq (Op2 (Var (Name "p")) Minus (Var (Name "m")))))]
         (Block [ Assign (Name "x") (Var (Name "m"))
                , Assign (Name "z") (Var (Name "p"))
                , While (Forall [] (Op2 (Op2 (Var (Name "x")) Ge (Val (IntVal 0)))
                                        Conj
                                        (Op2 (Op2 (Var (Name "z")) Minus (Var (Name "x")))
                                             Eq
                                             (Op2 (Var (Name "p")) Minus (Var (Name "m"))))))
                        (Op2 (Var (Name "x")) Gt (Val (IntVal 0)))
                        (Block [Assign (Name "z") (Op2 (Var (Name "z")) Minus (Val (IntVal 1))),
                                Assign (Name "x") (Op2 (Var (Name "x")) Minus (Val (IntVal 1)))])])

wArrayTest =
  Method "ArrayTest"
         [("a",TArrayInt)]
         [("x",TInt)]
         [ Requires (Forall [] (Op2 (Op1 Len (Var (Name "a"))) Gt (Val (IntVal 0))))
         , Ensures (Forall [] (Op2 (Var (Name "x")) Gt (Val (IntVal 0))))]
         (Block [Assign (Name "x") (Var (Proj "a" (Val (IntVal 0))))])

wLinear =
  Method "LinearSearch"
         [("a",TArrayInt),("key",TInt)]
         [("index",TInt)]
         [ Ensures (Forall [] (Op2 (Op2 (Op2 (Val (IntVal 0)) Le (Var (Name "index"))) Lt (Op1 Len (Var (Name "a")))) Implies (Op2 (Var (Proj "a" (Var (Name "index")))) Eq (Var (Name "key")))))
         , Ensures (PredOp (Forall [] (Op2 (Var (Name "index")) Lt (Val (IntVal 0))))
                           Implies
                           (Forall [("k",TInt)] (Op2 (Op2 (Op2 (Val (IntVal 0)) Le (Var (Name "k"))) Lt (Op1 Len (Var (Name "a")))) Implies (Op2 (Var (Proj "a" (Var (Name "k")))) Neq (Var (Name "key"))))))]
         (Block [ Assign (Name "index") (Val (IntVal (-1)))
                , Decl ("i",TInt) (Val (IntVal 0))
                , While (PredOp (Forall [] (Op2 (Var (Name "i")) Le (Op1 Len (Var (Name "a")))))
                                Conj
                                (PredOp (PredOp (Forall [] (Op2 (Var (Name "index")) Eq (Val (IntVal (-1)))))
                                         Implies
                                         (Forall [("k",TInt)] (Op2 (Op2 (Op2 (Val (IntVal 0)) Le (Var (Name "k"))) Lt (Var (Name "i"))) Implies (Op2 (Var (Proj "a" (Var (Name "k")))) Neq (Var (Name "key"))))))
                                        Conj
                                        (Forall [] (Op2 (Op2 (Op2 (Var (Name "index")) Ge (Val (IntVal 0))) Implies (Op2 (Var (Name "index")) Lt (Op1 Len (Var (Name "a"))))) Conj (Op2 (Var (Proj "a" (Var (Name "index")))) Eq (Var (Name "key")))))))
                        (Op2 (Var (Name "i")) Lt (Op1 Len (Var (Name "a"))))
                        (Block [If (Op2 (Var (Proj "a" (Var (Name "i")))) Eq (Var (Name "key")))
                                   (Block [Assign (Name "index") (Var (Name "i"))])
                                   (Block [])
                               , Assign (Name "i") (Op2 (Var (Name "i")) Plus (Val (IntVal 1)))])])

wMin = Method "Min" [("x",TInt),("y",TInt)] [("min",TInt)] [Ensures (Forall [] (Op2 (Op2 (Var (Name "min")) Le (Var (Name "x"))) Conj (Op2 (Var (Name "min")) Le (Var (Name "y")))))] (Block [If (Op2 (Var (Name "x")) Lt (Var (Name "y"))) (Block [Assign (Name "min") (Var (Name "x"))]) (Block [Assign (Name "min") (Var (Name "y"))])])

wSquareRoot = Method "SquareRoot" [("x",TInt)] [("z",TInt)] [Requires (Forall [] (Op2 (Var (Name "x")) Gt (Val (IntVal 0)))),Ensures (Forall [] (Op2 (Op2 (Op2 (Var (Name "z")) Times (Var (Name "z"))) Le (Var (Name "x"))) Conj (Op2 (Var (Name "x")) Lt (Op2 (Op2 (Var (Name "z")) Plus (Val (IntVal 1))) Times (Op2 (Var (Name "z")) Plus (Val (IntVal 1)))))))] (Block [Assign (Name "z") (Val (IntVal 0)),While (Forall [] (Op2 (Op2 (Var (Name "z")) Times (Var (Name "z"))) Le (Var (Name "x")))) (Op2 (Op2 (Op2 (Var (Name "z")) Plus (Val (IntVal 1))) Times (Op2 (Var (Name "z")) Plus (Val (IntVal 1)))) Le (Var (Name "x"))) (Block [Assign (Name "z") (Op2 (Var (Name "z")) Plus (Val (IntVal 1)))])])

wManyBinops = Method "ManyBinops" [("x",TInt),("y",TInt),("a",TBool),("b",TBool)] [("c",TInt)] [Requires (Forall [] (Op2 (Var (Name "y")) Neq (Val (IntVal 0))))] (Block [If (Op2 (Op2 (Op2 (Var (Name "a")) Conj (Var (Name "b"))) Disj (Op2 (Op1 Not (Var (Name "a"))) Conj (Op2 (Var (Name "x")) Lt (Var (Name "y"))))) Disj (Op2 (Var (Name "x")) Ge (Var (Name "y")))) (Block [Assign (Name "c") (Op2 (Op2 (Var (Name "x")) Plus (Var (Name "y"))) Minus (Op2 (Var (Name "x")) Divide (Var (Name "y"))))]) (Block [Assign (Name "c") (Op2 (Op2 (Var (Name "x")) Times (Var (Name "y"))) Modulo (Var (Name "y")))])])
