module Benchmark.Problems.CollatzNumbers where

import Benchmark.Core
import Benchmark.Metrics (intError)
import qualified Data.Map as M
import Data.Monoid (Sum (Sum))
import qualified Data.Set as S
import Evolution (randomR)
import Grammar

-- | WARNING: IMPOSSIBLE TO IMPLEMENT DUE TO POTENTIAL INFINITE LOOP (requires explicit recursion or iterateWhile)
collatzNumbers :: Benchmark (Sum Integer)
collatzNumbers =
  MkBenchmark
    { _benchmarkId = Nothing,
      _datasetName = "collatz-numbers",
      _inputTypes = [GInt],
      _outputType = GInt,
      _fitnessMetric = intError,
      _testCases = 2000,
      _trainCases = 200,
      _relevantTypes = S.fromList [GInt, GBool],
      _customOutputParser = Nothing,
      _allowedConstants =
        M.fromList
          [ (GBool, return . BoolLit <$> [True, False]),
            (GInt, [pure $ IntLit 0, pure $ IntLit 1, intRng])
          ]
    }
  where
    intRng = IntLit <$> randomR (-100, 100)