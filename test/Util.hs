module Util
  ( isSorted
  , only
  ) where

import Test.QuickCheck

isSorted :: (Ord a, Show a) => [a] -> Property
isSorted xs = counterexample ("isSorted " ++ show xs) (isSorted_ xs)
{-# INLINABLE isSorted #-}

isSorted_ :: Ord a => [a] -> Bool
isSorted_ []      = True
isSorted_ (x0:xs) = go x0 xs
  where
    go _ []     = True
    go x (y:ys) = x <= y && go y ys

only :: [a] -> a
only [x] = x
only _   = error "only"
