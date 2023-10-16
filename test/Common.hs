module Common
  ( sortAp
  , sortAlt
  ) where

import Control.Monad.Trans.Writer.Lazy
import Data.Foldable
import Data.Maybe

import qualified Control.Applicative.Prio as PrioAp
import qualified Control.Alternative.Prio as PrioAlt

sortAp :: Ord a => [a] -> [a]
sortAp =
  execWriter .
  PrioAp.unlift .
  traverse_ (\x -> PrioAp.lift x (tell [x]))
{-# INLINABLE sortAp #-}

sortAlt :: Ord a => [a] -> [a]
sortAlt =
  fromJust .
  execWriterT .
  PrioAlt.unlift .
  traverse_ (\x -> PrioAlt.lift x (tell [x]))
{-# INLINABLE sortAlt #-}
