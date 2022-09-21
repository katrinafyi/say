{-# LANGUAGE LambdaCase #-}
module Lib
    ( someFunc
    ) where

import Data.Functor
import Data.Bifunctor
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.List (unfoldr)



takeWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
takeWhileM _ [] = pure []
takeWhileM f (x:xs) = do
  cont <- f x
  if cont then
    (x:) <$> takeWhileM f xs 
  else 
    pure []

test :: Int -> [a] -> [a]
test n0 xs = evalState (takeWhileM go xs) n0
  where 
    go :: a -> State Int Bool
    go _ = do
      n <- get 
      if n > 0 then 
        modify (subtract 1) $> True 
      else 
        pure False

softWrap :: Int -> [[a]] -> ([[a]], [[a]])
softWrap width xs = (row, drop (length row) xs)
  where
    row = evalState (takeWhileM go xs) 0 
    go :: [a] -> State Int Bool 
    go word = do 
      wd <- get
      when (wd /= 0) $ modify (+1)
      wd' <- gets (+ length word)
      put wd'
      pure (wd' <= width)


hardWrap :: Int -> [[a]] -> ([[a]], [[a]])
hardWrap wd (x:xs) = ([take wd x], drop wd x:xs)
hardWrap _ [] = ([], [])


wrap :: Int -> [[a]] -> [[[a]]]
wrap _ [] = []
wrap wd xs = row : wrap wd rest
  where
    (row, rest) = wrap' wd wd xs

wrap' :: Int -> Int -> [[a]] -> ([[a]], [[a]])
wrap' hardwd wd [] = ([], [])
wrap' hardwd wd (w:xs)
  | l > hardwd = ([take wd w], drop wd w : xs)
  | l < wd = Data.Bifunctor.first (w:) rest
  | otherwise = ([], w:xs)
  where 
    l = length w
    rest = wrap' hardwd (wd - l - 1) xs


wrap2 :: Int -> [[a]] -> [[[a]]]
wrap2 _ [] = [[]]
wrap2 wd xs = unfoldr go xs
  where 
    go [] = Nothing
    go ws = Just (wrap' wd wd ws)

box :: [String] -> [String]
box [] = ["+-+", "| |", "+-+"]
box rows = [hr] ++ body ++ [hr]
  where
    wd = maximum $ length <$> rows
    pad s = s ++ replicate (wd - length s) ' '

    hr = "+" ++ replicate wd '-' ++ "+"
    body = fmap (\s -> "|" ++ pad s ++ "|") rows


someFunc :: IO ()
someFunc = do
  let wd = 40
  putStrLn $ replicate wd '-'
  -- print $ fmap unwords . wrap2 wd . words <$> lines "The package can be used on its own in portable Haskell code, in which case operations need to be manually lifted through transformer stacks (see Control.Monad.Trans.Class for some examples). Alternatively, it can be used with the non-portable monad classes in the mtl or monads-tf packages, which automatically lift operations introduced by monad transformers through other transformers.\n\n\nAn example from The Craft of Functional Programming, Simon Thompson (http://www.cs.kent.ac.uk/people/staff/sjt/), Addison-Wesley 1999: Given an arbitrary tree, transform it to a tree of integers in which the original elements are replaced by natural numbers, starting from 0. The same element has to be replaced by the same number at every occurrence, and when we meet an as-yet-unvisited element we have to find a 'new' number to match it with:"
  putStrLn $ unlines $ box $ concat $ fmap unwords . wrap2 wd . words <$> lines "The package can be used on its own in portable Haskell code, in which case operations need to be manually lifted through transformer stacks (see Control.Monad.Trans.Class for some examples). Alternatively, it can be used with the non-portable monad classes in the mtl or monads-tf packages, which automatically lift operations introduced by monad transformers through other transformers.\n\n\nAn example from The Craft of Functional Programming, Simon Thompson (http://www.cs.kent.ac.uk/people/staff/sjt/), Addison-Wesley 1999: Given an arbitrary tree, transform it to a tree of integers in which the original elements are replaced by natural numbers, starting from 0. The same element has to be replaced by the same number at every occurrence, and when we meet an as-yet-unvisited element we have to find a 'new' number to match it with:"

