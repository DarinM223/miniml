import CpsConvert qualified
import Optimization.Contract qualified as Contract
import Optimization.Eta qualified as Eta
import Optimization.Expand qualified as Expand
import Optimization.Flatten qualified as Flatten
import Optimization.Uncurry qualified as Uncurry
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ CpsConvert.tests,
      Contract.tests,
      Flatten.tests,
      Eta.tests,
      Uncurry.tests,
      Expand.tests
    ]