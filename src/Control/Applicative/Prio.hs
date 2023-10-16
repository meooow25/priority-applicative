{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- See the package README for usage.

module Control.Applicative.Prio
  ( PrioAp
  , lift
  , unlift
  , hoist
  , Some(..)
  , minView
  , toList
  ) where

import Control.Applicative (liftA2)
import Data.List (unfoldr)

data PrioAp p f a
  = Pure a
  | forall z. Cons !(PrioAp p f (z -> a)) !p (f z)

-- | Lift an effect with a priority. Strict in the priority.
lift :: p -> f a -> PrioAp p f a
lift = Cons (Pure id)

-- | Unlift into an applicative functor.
unlift :: Applicative f => PrioAp p f a -> f a
unlift (Pure x)       = pure x
unlift (Cons tx _ fx) = liftA2 (flip ($)) fx (unlift tx)
{-# INLINABLE unlift #-}

instance Functor (PrioAp p f) where
  fmap f (Pure x)        = Pure (f x)
  fmap f (Cons tx px fx) = Cons (fmap (f .) tx) px fx
  {-# INLINABLE fmap #-}

  y <$ Pure _        = Pure y
  y <$ Cons tx px fx = Cons (const y <$ tx) px fx
  {-# INLINABLE (<$) #-}

instance Ord p => Applicative (PrioAp p f) where
  pure = Pure

  Pure x <*> r = fmap x r
  l <*> Pure y = fmap ($ y) l
  l@(Cons tx px fx) <*> r@(Cons ty py fy)
    | px <= py  = Cons (liftA2 flip tx r) px fx
    | otherwise = Cons (liftA2 (.) l ty) py fy
  {-# INLINABLE (<*>) #-}

  Pure _ *> r = r
  l *> Pure y = y <$ l
  l@(Cons tx px fx) *> r@(Cons ty py fy)
    | px <= py  = Cons (tx *> (const <$> r)) px fx
    | otherwise = Cons (l *> ty) py fy
  {-# INLINABLE (*>) #-}

  (<*) = flip (*>)
  {-# INLINABLE (<*) #-}

-- | Use a natural transformation to change the @PrioAp@'s base functor.
hoist :: (forall a. f a -> g a) -> PrioAp p f b -> PrioAp p g b
hoist _ (Pure x)        = Pure x
hoist f (Cons tx px fx) = Cons (hoist f tx) px (f fx)

-- | Existential type containing some @f a@.
data Some f = forall a. Some (f a)

-- | Split the @PrioAp@ into the minimum priority effect and the rest.
minView :: PrioAp p f a -> Maybe ((p, Some f), Some (PrioAp p f))
minView (Pure _)        = Nothing
minView (Cons tx px fx) = Just ((px, Some fx), Some tx)
{-# INLINABLE minView #-}

-- | The effects and priorities in the @PrioAp@, in priority order.
toList :: PrioAp p f a -> [(p, Some f)]
toList = unfoldr (\(Some tx) -> minView tx) . Some
{-# INLINE toList #-}
