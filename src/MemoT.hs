{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MemoT where

import           Control.Monad.Trans.State.Strict
import           Data.Function                  ( fix )
import           Data.Functor.Identity          ( Identity(runIdentity) )

import           Control.Applicative            ( Applicative(liftA2) )
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Identity   ( runIdentityT )
import           Data.Map                       ( Map )
import qualified Data.Map.Strict               as Map

-- | Type of recursive functions with explicit self reference.
type OpenFun a b = (a -> b) -> a -> b

fib :: (Integer -> Integer) -> Integer -> Integer
fib rec n = if n <= 1 then 1 else rec (n - 1) + rec (n - 2)

type Memoisable m a b
  = forall t . (MonadTrans t, Monad (t m)) => OpenFun a (t m b)

fib2 :: Memoisable Identity Integer Integer
fib2 rec n =
  if n <= 1 then pure 1 else liftA2 (+) (rec (n - 1)) (rec (n - 2))

fib3 :: Memoisable IO Integer Integer
fib3 rec n = 
  lift (print n) >>
  if n <= 1 then 
    lift $ pure 1
  else
    liftA2 (+) (rec (n - 1)) (rec (n - 2))


type Memoised m a b = a -> StateT (Map a b) m b

naive :: (forall m . Memoisable m a b) -> a -> b
naive f = runIdentity . runIdentityT . fix f

nomemoise :: (Monad m) => Memoisable m a b -> Memoised m a b
nomemoise f = fix f

memoise :: (Monad m, Ord a) => Memoisable m a b -> Memoised m a b
memoise f a = do
  old <- gets (Map.lookup a)
  maybe compute pure old
 where
  compute = do
    x <- f (memoise f) a
    modify (Map.insert a x)
    pure x

nomemoExample :: [Integer]
nomemoExample =
  runIdentity $ evalStateT (traverse (nomemoise fib2) [1 .. 30]) Map.empty

memoExample :: [Integer]
memoExample =
  runIdentity $ evalStateT (traverse (memoise fib2) [1 .. 30]) Map.empty


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
