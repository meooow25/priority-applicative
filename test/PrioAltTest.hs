{-# LANGUAGE DeriveGeneric #-}
module PrioAltTest (tests) where

import Control.Applicative (Alternative(..), liftA2)
import Control.Monad
import Control.Monad.Trans.Writer.Lazy
import Data.Maybe
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Tree as T
import GHC.Generics

import Test.QuickCheck.Poly
import Test.Tasty
import Test.Tasty.QuickCheck

import Control.Alternative.Prio (Some(..))
import qualified Control.Alternative.Prio as PrioAlt
import Common (sortAlt)
import Util (isSorted, only)

tests :: TestTree
tests = testGroup "Control.Alternative.Prio"
  [ testProperty "sort" prop_sort
  , testProperty "lift, unlift, toTree via WriterT []" prop_writerTList
  ]

prop_sort :: [Int] -> Property
prop_sort xs = sortAlt xs === L.sort xs

prop_writerTList :: AltTree OrdB Maybe OrdA -> Property
prop_writerTList t =
  -- prio and writer results and written values match
  normalize respxs' === normalize respxs .&&.
  -- prio written values sorted by priority
  conjoin (map (isSorted . map fst . snd) respxs') .&&.
  -- prio toTree is sorted
  isTreeSorted (map (fmap fst) tr) .&&.
  -- prio toTree priorities match prio toTree written priorities
  conjoin [p === p' | (p, Just (p', _)) <- ppxs]
  where
    toW :: OrdB -> Maybe OrdA -> WriterT [(OrdB, OrdA)] [] OrdA
    toW p = maybe empty (\x -> x <$ tell [(p, x)])
    respxs = runWriterT $ runAltTree toW t
    prio = runAltTree (\p -> PrioAlt.lift p . toW p) t
    respxs' = runWriterT $ PrioAlt.unlift prio
    tr = PrioAlt.toTree prio
    ppxs = map (\(p, Some w) -> (p, fmap only (zeroOrOne (execWriterT w)))) (concatMap F.toList tr)

zeroOrOne :: [a] -> Maybe a
zeroOrOne []  = Nothing
zeroOrOne [x] = Just x
zeroOrOne _   = error "zeroOrOne"

normalize :: (Ord p, Ord a) => [(a, [(p, a)])] -> [(a, [(p, a)])]
normalize = L.sort . map (\(x, ys) -> (x, L.sort ys))

isTreeSorted :: (Ord a, Show a) => [T.Tree a] -> Property
isTreeSorted []  = property True
isTreeSorted ts0 = counterexample ("isSorted: " ++ show ts0) (isJust (go ts0))
  where
    go ts = foldr1 cmp (map go' ts)
    go' (T.Node x ts) = foldr1 cmp (Just x : map go' ts)
    cmp mx my = do
      x <- mx
      y <- my
      x <$ guard (x <= y)

data AltTree p f a
  = Lift p (f a)
  | Pure a
  | Fmap (Fun a a) (AltTree p f a)
  | FmapConst a (AltTree p f a)
  | LiftA2 (Fun (a, a) a) (AltTree p f a) (AltTree p f a)
  | Then (AltTree p f a) (AltTree p f a)
  | Neht (AltTree p f a) (AltTree p f a)
  | Empty
  | Alt (AltTree p f a) (AltTree p f a)
  deriving (Show, Generic)

runAltTree :: Alternative g => (p -> f a -> g a) -> AltTree p f a -> g a
runAltTree g = go
  where
    go (Lift p x)           = g p x
    go (Pure x)             = pure x
    go (Fmap (Fn f) t)      = fmap f (go t)
    go (FmapConst a t)      = a <$ go t
    go (LiftA2 (Fn2 f) l r) = liftA2 f (go l) (go r)
    go (Then tx ty)         = go tx *> go ty
    go (Neht tx ty)         = go tx <* go ty
    go Empty                = empty
    go (Alt tx ty)          = go tx <|> go ty

instance (Arbitrary p, Arbitrary a, Arbitrary (f a), CoArbitrary a, Function a) =>
  Arbitrary (AltTree p f a) where
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
          , pure Empty
          ]
        else oneof
          [ Fmap <$> arbitrary <*> go (n - 1)
          , FmapConst <$> arbitrary <*> go (n - 1)
          , LiftA2 <$> arbitrary <*> go nx <*> go ny
          , Then <$> go nx <*> go ny
          , Neht <$> go nx <*> go ny
          , Alt <$> go nx <*> go ny
          ]

  shrink = genericShrink
