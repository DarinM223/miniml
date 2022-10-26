import CpsConvert qualified
import Optimization.Contract qualified as Contract
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ CpsConvert.tests,
      Contract.tests
    ]