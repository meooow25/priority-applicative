{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- See the package README for usage.

module Control.Alternative.Prio
  ( PrioAlt
  , lift
  , unlift
  , hoist
  , Some(..)
  , minView
  , toTree
  ) where

import Control.Applicative (Alternative(..), liftA2)
import Data.Maybe (mapMaybe)
import qualified Data.Tree as T

import Control.Applicative.Prio (Some(..))

newtype PrioAlt p f a = Choice { unChoice :: [PrioNode p f a] }

data PrioNode p f a
  = Pure a
  | forall z. Node !(PrioAlt p f (z -> a)) !p (f z)

-- | Lift an effect with a priority. Strict in the priority.
lift :: p -> f a -> PrioAlt p f a
lift !px fx = Choice [Node (Choice [Pure id]) px fx]

-- | Unlift into an applicative functor.
unlift :: Alternative f => PrioAlt p f a -> f a
unlift = foldr ((<|>) . unliftNode) empty . unChoice

unliftNode :: Alternative f => PrioNode p f a -> f a
unliftNode (Pure x)       = pure x
unliftNode (Node tx _ fx) = liftA2 (flip ($)) fx (unlift tx)

instance Functor (PrioAlt p f) where
  fmap f = Choice . fmap (fmap f) . unChoice
  {-# INLINABLE fmap #-}

  (<$) y = Choice . fmap (y <$) . unChoice
  {-# INLINABLE (<$) #-}

instance Functor (PrioNode p f) where
  fmap f (Pure x)        = Pure (f x)
  fmap f (Node tx px fx) = Node (fmap (f .) tx) px fx
  {-# INLINABLE fmap #-}

  y <$ Pure _        = Pure y
  y <$ Node tx px fx = Node (const y <$ tx) px fx
  {-# INLINABLE (<$) #-}

instance Ord p => Applicative (PrioAlt p f) where
  pure x = Choice [Pure x]

  Choice nxs <*> Choice nys = Choice (go nxs nys)
    where
      go _  [] = []
      go [] _  = []
      go (Pure x : ls') rs = unChoice $ Choice (fmap (fmap x) rs) <|> Choice (go ls' rs)
      go ls (Pure y : rs') = unChoice $ Choice (fmap (fmap ($ y)) ls) <|> Choice (go ls rs')
      go ls@(Node tx px fx : ls') rs@(Node ty py fy : rs')
        | px <= py  = Node (liftA2 flip tx (Choice rs)) px fx : go ls' rs
        | otherwise = Node (liftA2 (.) (Choice ls) ty) py fy : go ls rs'
  {-# INLINABLE (<*>) #-}

  Choice nxs *> Choice nys = Choice (go nxs nys)
    where
      go _ [] = []
      go [] _ = []
      go (Pure _ : ls') rs = unChoice $ Choice rs <|> Choice (go ls' rs)
      go ls (Pure y : rs') = unChoice $ Choice (fmap (y <$) ls) <|> Choice (go ls rs')
      go ls@(Node tx px fx : ls') rs@(Node ty py fy : rs')
        | px <= py  = Node (tx *> (const <$> Choice rs)) px fx : go ls' rs
        | otherwise = Node (Choice ls *> ty) py fy : go ls rs'
  {-# INLINABLE (*>) #-}

  (<*) = flip (*>)
  {-# INLINABLE (<*) #-}

instance Ord p => Alternative (PrioAlt p f) where
  empty = Choice []

  Choice nxs <|> Choice nys = Choice (go nxs nys)
    where
      go ls [] = ls
      go [] rs = rs
      go (l@(Pure _) : ls') rs = l : go ls' rs
      go ls (r@(Pure _) : rs') = r : go ls rs'
      go ls@(l@(Node _ px _) : ls') rs@(r@(Node _ py _) : rs')
        | px <= py  = l : go ls' rs
        | otherwise = r : go ls rs'
  {-# INLINABLE (<|>) #-}

-- | Use a natural transformation to change the @PrioAlt@'s base functor.
hoist :: (forall a. f a -> g a) -> PrioAlt p f b -> PrioAlt p g b
hoist f = Choice . map (hoistNode f) . unChoice

hoistNode :: (forall a. f a -> g a) -> PrioNode p f b -> PrioNode p g b
hoistNode _ (Pure x)        = Pure x
hoistNode f (Node tx px fx) = Node (hoist f tx) px (f fx)

-- | Split a @PrioAlt@ into its branches. A branch is returned as a tuple of the minimum priority
-- effect in it and the rest of it.
minView :: PrioAlt p f a -> [((p, Some f), Some (PrioAlt p f))]
minView = mapMaybe minViewNode . unChoice

minViewNode :: PrioNode p f a -> Maybe ((p, Some f), Some (PrioAlt p f))
minViewNode (Pure _)        = Nothing
minViewNode (Node tx px fx) = Just ((px, Some fx), Some tx)

-- | The effects and priorities in the @PrioAlt@, in priority order from
-- root to leaf.
toTree :: PrioAlt p f a -> [T.Tree (p, Some f)]
toTree = mapMaybe toTreeNode . unChoice

toTreeNode :: PrioNode p f a -> Maybe (T.Tree (p, Some f))
toTreeNode (Pure _)        = Nothing
toTreeNode (Node tx px fx) = Just (T.Node (px, Some fx) (toTree tx))
