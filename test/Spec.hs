import qualified BenchmarkSpec
import qualified EvolutionSpec
import qualified GrammarSpec
import qualified MaxTreeDepthSpec
import qualified MeasureSpec
import qualified PrettyTreeSpec
import Test.Tasty

main =
  defaultMain $
    testGroup
      "Tests"
      [ testGroup "Grammar" GrammarSpec.tests,
        testGroup "Evolution" EvolutionSpec.tests,
        testGroup "Benchmark" BenchmarkSpec.tests,
        testGroup "Max tree depth" MaxTreeDepthSpec.tests,
        testGroup "Prettify Tree" PrettyTreeSpec.tests,
        testGroup "Measure" MeasureSpec.tests
      ]
