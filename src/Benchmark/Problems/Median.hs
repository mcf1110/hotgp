module Benchmark.Problems.Median where

import Benchmark.Core
import Benchmark.Metrics (rightWrong)
import qualified Data.Map as M
import Data.Monoid (Sum (Sum))
import qualified Data.Set as S
import Evolution (randomR)
import Grammar

median :: Benchmark (Sum Integer)
median =
  MkBenchmark
    { _benchmarkId = Nothing,
      _datasetName = "median",
      _inputTypes = [GInt, GInt, GInt],
      _outputType = GInt,
      _fitnessMetric = rightWrong,
      _testCases = 1000,
      _trainCases = 100,
      _relevantTypes = S.fromList [GInt, GBool],
      _customOutputParser = Nothing,
      _allowedConstants =
        M.fromList
          [(GInt, [IntLit <$> randomR (-100, 100)])]
    }
