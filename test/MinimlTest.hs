import Closure.Convert qualified as Convert
import Closure.Free qualified as Free
import CpsConvert qualified
import Optimization.Contract qualified as Contract
import Optimization.Cse qualified as Cse
import Optimization.Eta qualified as Eta
import Optimization.Expand qualified as Expand
import Optimization.Flatten qualified as Flatten
import Optimization.Hoist qualified as Hoist
import Optimization.Uncurry qualified as Uncurry
import Spill qualified
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
      Expand.tests,
      Hoist.tests,
      Cse.tests,
      Free.tests,
      Convert.tests,
      Spill.tests
    ]