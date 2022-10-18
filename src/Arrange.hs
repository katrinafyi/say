{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Arrange where 

import GHC.TypeNats (type (+), type (<=), Nat)
import Data.Kind
import Data.Proxy (Proxy (Proxy))


data Align = Start | Middle | End

data List2 :: Nat -> Nat -> Type -> Type where 
  Line :: [a] -> List2 w 1 a
  HPad :: (w <= w') => Align -> List2 w h a -> List2 w' h a
  VPad :: (h <= h') => Align -> List2 w h a -> List2 w h' a

data Exists t = forall w. Exists { getExist :: Proxy (w::t)}

proxyAdd :: Proxy (x::Nat) -> Proxy (x + 1)
proxyAdd _ = Proxy

proxyLen :: [a] -> Exists Nat
proxyLen [] = Exists (Proxy :: Proxy 0)
proxyLen (_:xs) =
  case proxyLen xs of 
  Exists p -> Exists (proxyAdd p)


-- proxyLine :: Proxy w -> [a] -> List2 w 1 a

class ToInt p where 
  toInt :: p -> Int


-- instance (ToInt (Proxy n), m ~ n + 1) => ToInt (Proxy m) where
--   toInt :: (ToInt (Proxy n), m ~ (n + 1)) => Proxy m -> Int
--   toInt p = 1 + toInt (Proxy :: Proxy n)



getNat :: Exists Nat -> Int
getNat (Exists p) = 0

l :: List2 w 1 Char
l = Line "asdf asdf asdf"

l2 :: List2 20 1 Char
l2 = HPad Start (l:: List2 10 1 Char)

main :: IO ()
main = do 
  pure ()

