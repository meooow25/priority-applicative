import Test.Tasty.Bench

import Control.DeepSeq
import Control.Exception
import qualified Data.List as L

import System.Random

import Common (sortAp, sortAlt)

main :: IO ()
main = do
  xs <- evaluate (force randomList)
  defaultMain
    [ bench "sortAp" $ nf sortAp xs
    , bench "sortAlt" $ nf sortAlt xs
    , bench "List.sort" $ nf L.sort xs
    , bench "insSort" $ nf insSort xs
    ]

randomList :: [Int]
randomList = take 500 $ randoms $ mkStdGen 42

insSort :: Ord a => [a] -> [a]
insSort = L.foldl' (flip L.insert) []
