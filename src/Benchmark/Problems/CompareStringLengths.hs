module Benchmark.Problems.CompareStringLengths (compareStringLengths) where

import Benchmark.Core
import Benchmark.Helpers
import Benchmark.Metrics (boolError)
import qualified Data.Map as M
import Data.Monoid (Sum (Sum))
import qualified Data.Set as S
import Evolution (randomR)
import Grammar

compareStringLengths :: Benchmark (Sum Integer)
compareStringLengths =
  MkBenchmark
    { _benchmarkId = Nothing,
      _datasetName = "compare-string-lengths",
      _inputTypes = [GList GChar, GList GChar, GList GChar],
      _outputType = GBool,
      _fitnessMetric = boolError,
      _testCases = 1000,
      _trainCases = 100,
      _relevantTypes = S.fromList [GInt, GBool, GList GChar],
      _customOutputParser = Nothing,
      _allowedConstants =
        M.fromList
          [ (GBool, return . BoolLit <$> [True, False])
          ]
    }
