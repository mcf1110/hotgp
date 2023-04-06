module Benchmark.Problems.SumOfSquares where

import Benchmark.Core
import Benchmark.Helpers (allowList, allowUnaryLambdas)
import Benchmark.Metrics (intError)
import qualified Data.Map as M
import Data.Monoid (Sum (Sum))
import qualified Data.Set as S
import Evolution (randomR)
import Grammar

sumOfSquares :: Benchmark (Sum Integer)
sumOfSquares =
  MkBenchmark
    { _benchmarkId = Nothing,
      _datasetName = "sum-of-squares",
      _inputTypes = [GInt],
      _outputType = GInt,
      _fitnessMetric = intError,
      _testCases = 50,
      _trainCases = 50,
      _relevantTypes = S.fromList $ (allowUnaryLambdas <> allowList) [GInt, GBool],
      _customOutputParser = Nothing,
      _allowedConstants = M.fromList [(GInt, [intRng, pure $ IntLit 0, pure $ IntLit 1])]
    }
  where
    intRng = IntLit <$> randomR (-100, 100)