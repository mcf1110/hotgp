module Benchmark.Problems.NegativeToZero where

import Benchmark.Core
import Benchmark.Helpers (allowList, allowUnaryLambdas)
import Benchmark.Metrics (levenshteinDistance)
import qualified Data.Map as M
import Data.Monoid (Sum (Sum))
import qualified Data.Set as S
import Grammar

negativeToZero :: Benchmark (Sum Integer)
negativeToZero =
  MkBenchmark
    { _benchmarkId = Nothing,
      _datasetName = "negative-to-zero",
      _inputTypes = [GList GInt],
      _outputType = GList GInt,
      _fitnessMetric = levenshteinDistance,
      _testCases = 2000,
      _trainCases = 200,
      _relevantTypes = S.fromList $ (allowUnaryLambdas <> allowList) [GInt, GBool],
      _customOutputParser = Nothing,
      _allowedConstants =
        M.fromList
          [(GInt, [pure $ IntLit 0])]
    }
