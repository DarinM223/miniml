import CpsConvert qualified
import Optimization.Contract qualified as Contract
import Optimization.Eta qualified as Eta
import Optimization.Flatten qualified as Flatten
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
      Eta.tests
    ]