{-# LANGUAGE DeriveGeneric #-}
module PrioApTest (tests) where

import Control.Applicative (liftA2)
import Control.Monad.Trans.Writer.Lazy
import Data.Functor.Identity
import qualified Data.List as L
import GHC.Generics

import Test.QuickCheck.Poly
import Test.Tasty
import Test.Tasty.QuickCheck

import Control.Applicative.Prio (Some(..))
import qualified Control.Applicative.Prio as PrioAp
import Common (sortAp)
import Util (isSorted, only)

tests :: TestTree
tests = testGroup "Control.Applicative.Prio"
  [ testProperty "sort" prop_sort
  , testProperty "lift, unlift, toList via Writer" prop_writer
  ]

prop_sort :: [Int] -> Property
prop_sort xs = sortAp xs === L.sort xs

prop_writer :: ApTree OrdB Identity OrdA -> Property
prop_writer t =
  -- prio and writer results match
  res' === res .&&.
  -- prio written values sorted by priority
  isSorted (map fst pxs') .&&.
  -- prio and writer written values match
  L.sort pxs' === L.sort pxs .&&.
  -- prio toList priorities match prio toList written priorities
  conjoin [p === p' | (p, (p', _)) <- ppxs] .&&.
  -- prio toList written values match prio written values
  map snd ppxs === pxs'
  where
    toW :: OrdB -> Identity OrdA -> Writer [(OrdB, OrdA)] OrdA
    toW p (Identity x) = x <$ tell [(p, x)]
    (res, pxs) = runWriter $ runApTree toW t
    prio = runApTree (\p -> PrioAp.lift p . toW p) t
    (res', pxs') = runWriter $ PrioAp.unlift prio
    ppxs = map (\(p, Some w) -> (p, only (execWriter w))) (PrioAp.toList prio)

data ApTree p f a
  = Lift p (f a)
  | Pure a
  | Fmap (Fun a a) (ApTree p f a)
  | FmapConst a (ApTree p f a)
  | LiftA2 (Fun (a, a) a) (ApTree p f a) (ApTree p f a)
  | Then (ApTree p f a) (ApTree p f a)
  | Neht (ApTree p f a) (ApTree p f a)
  deriving (Show, Generic)

runApTree :: Applicative g => (p -> f a -> g a) -> ApTree p f a -> g a
runApTree g = go
  where
    go (Lift p x)           = g p x
    go (Pure x)             = pure x
    go (Fmap (Fn f) t)      = fmap f (go t)
    go (FmapConst a t)      = a <$ go t
    go (LiftA2 (Fn2 f) l r) = liftA2 f (go l) (go r)
    go (Then tx ty)         = go tx *> go ty
    go (Neht tx ty)         = go tx <* go ty

instance (Arbitrary p, Arbitrary a, Arbitrary (f a), CoArbitrary a, Function a) =>
  Arbitrary (ApTree p f a) where
  arbitrary = sized go
    where
      go n' = do
        n <- choose (0, n')
        let nx = n `div` 2
            ny = n - nx
        if n == 0
        then oneof
          [ Pure <$> arbitrary
          , Lift <$> arbitrary <*> arbitrary
          ]
        else oneof
          [ Fmap <$> arbitrary <*> go (n - 1)
          , FmapConst <$> arbitrary <*> go (n - 1)
          , LiftA2 <$> arbitrary <*> go nx <*> go ny
          , Then <$> go nx <*> go ny
          , Neht <$> go nx <*> go ny
          ]

  shrink = genericShrink
