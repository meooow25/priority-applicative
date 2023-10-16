import Test.Tasty

import qualified PrioApTest
import qualified PrioAltTest

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ PrioApTest.tests
  , PrioAltTest.tests
  ]
