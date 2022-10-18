{-# LANGUAGE RankNTypes #-}

module Memo where

import           Control.Monad.Trans.State.Strict
import           Data.Function                  ( fix )
import           Data.Functor.Identity          ( Identity(runIdentity) )

import           Data.Map                       ( Map )
import qualified Data.Map.Strict               as Map

-- | Type of recursive functions with explicit self reference.
type OpenFun a b = (a -> b) -> a -> b

fib :: (Integer -> Integer) -> Integer -> Integer
fib rec n = if n <= 1 then 1 else rec (n - 1) + rec (n - 2)

fib2 :: Monad m => (Integer -> m Integer) -> Integer -> m Integer
fib2 rec n = if n <= 1 then pure 1 else (+) <$> rec (n - 1) <*> rec (n - 2)

type Memoisable a b = forall m . Monad m => OpenFun a (m b)

naive :: Memoisable a b -> a -> b
naive f = runIdentity . fix f

type Memoised a b = Ord a => a -> State (Map a b) b

nomemoise :: Memoisable a b -> Memoised a b
nomemoise f = pure . naive f

memoise :: Memoisable a b -> Memoised a b
memoise f a = gets (Map.lookup a) >>= maybe compute pure
 where
  compute = do
    x <- f (memoise f) a
    modify (Map.insert a x)
    pure x

{-
example output:

ghci> :set +s

ghci> naive fib2 <$> [1..30]
[1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368,75025,121393,196418,317811,514229,832040,1346269]
(6.55 secs, 3,158,679,408 bytes)

ghci> evalState (traverse (memoise fib2) [1..30]) Map.empty
[1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368,75025,121393,196418,317811,514229,832040,1346269]
(0.01 secs, 840,352 bytes)
-}