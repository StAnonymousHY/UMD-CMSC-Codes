module Prep where

{- 
The definitions of `StateT`, `ExceptT` and `Identity` come from separate modules
in the `mtl` library.
-}

import Data.Function ((&))
import Control.Monad.Identity 
import Control.Monad.State
import Control.Monad.Except
import Data.Char (isAlpha)

import Control.Applicative
import Data.Map (Map, (!?))
import qualified Data.Map as Map

-- Here is the state monad and some operations.

-- newtype State s a = S {runState :: s -> (a, s)}
-- get :: State s s
-- put :: s -> State s ()
-- modify :: (s -> s) -> State s ()

-- What is the type of:
  -- get >>= put

-- get :: State s s 
-- >>= :: m a -> (a -> m b) -> m b
-- put :: s -> State s ()
-- m := State s
-- a := s
-- b := ()

ex1 :: State s () 
ex1 = get >>= put
  
  -- modify id

-- return <$> get
-- return :: x -> m x
-- <$> :: (a -> b) -> f a -> f b
-- get : State s s

-- x = a  
-- m x = b
-- f = State s

ex3 :: Monad m => State s (m s)
ex3 = return <$> get 

-- What is a term of type:
  -- State a [a]

ex2 :: State a [a]
ex2 = return <$> get
--ex2 = do
--  a <- get
--  return [a]

-- (a -> b) -> State a b
ex4 :: (a -> b) -> State a b
ex4 = (<$> get)
  
-- \f -> f <$> get
  
-- do 
--   a <- get
--   return (f a)

ex5 :: [a] -> [a]
ex5 l = [ x | x <- l ]

ex6 :: [Int] -> [Int]
ex6 l = [ x+1 | x <- l ]

ex7 :: [a] -> [(a,a)]
ex7 l = [ (x,x) | x <- l ]

ex8 :: [a] -> [b] -> [(a,b)]
ex8 l1 l2 = [ (x,y) | x <- l1, y <- l2 ]

ex9 :: [Int] -> [Int] -> [Int]
ex9 l1 l2 = [ x + y | x <- l1, y <- l2 ]

-- What is ex8 [1,2] [3,4]?

foo :: Monad m => (a -> m b) -> [a] -> m [b]
foo f [] = return []
foo f (x:xs) = do
  y <- f x
  ys <- foo f xs
  return (y:ys)

-- What is foo Just [1..4]?
-- What is foo (\n -> if even n then Just n else Nothing) [1..4]?
-- What is foo (\n -> if even n then [n] else []) [1..4]
-- What is foo (\n -> if even n then [n,n] else [n]) [1..3]

replM :: Monad m => Int -> m a -> m [a] 
replM 0 m = return []
replM n m = do
  a <- m
  as <- replM (n-1) m
  return (a:as)

ex10 :: State Int [Int]
ex10 = replM 4 (get)  

ex15 :: State Bool [Bool]
ex15 = replM 4 (get)

ex16 :: State a [a]
ex16 = replM 4 (get)

fo :: Monad m => (a -> m Bool) -> [a] -> m [a]
fo p [] = return []
fo p (x : xs) = do
  b <- p x
  ys <- fo p xs
  return $ if b then (x : ys) else ys

bar l1 l2 = [ x + y | x <- l1, y <- l2 ]

bar3 l1 l2 l3 = [ x + y + z | x <- l1, y <- l2, z <- l3 ]

bar3' l1 l2 l3 =
  let l = bar l1 l2
  in bar l l3


ex11 :: Monad m => (a -> m b) -> a -> m [b]
ex11 = \f a -> pure <$> f a
-- do  b <- f a
--     return [b]

ex17 :: (Monad m, Applicative f) => (a -> m b) -> a -> m (f b)
ex17 f a = pure <$> f a

ex12 :: Monad m => (a -> m (b,c)) -> a -> m c
ex12 = \f a -> do
  (b,c) <- f a
  return c

ex13 :: Monad m => (a -> b -> m c) -> (a,b) -> m [c]
ex13 = \f (a,b) -> do
  c <- f a b
  return [c]

ex14 :: Monad m => (a -> m b) -> (b -> m [c]) -> a -> m [c]
ex14 = \f g a -> do
  b <- f a
  cs <- g b
  return cs

ex18 :: Monad m => State s (m s)
ex18 = do
  a <- get
  return (return a)

