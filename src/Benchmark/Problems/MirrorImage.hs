module Benchmark.Problems.MirrorImage where

import Benchmark.Core
import Benchmark.Helpers (allowList, allowPairs, allowUnaryLambdas)
import Benchmark.Metrics (boolError)
import qualified Data.Map as M
import Data.Monoid (Sum (Sum))
import qualified Data.Set as S
import Evolution (randomR)
import Grammar

mirrorImage :: Benchmark (Sum Integer)
mirrorImage =
  MkBenchmark
    { _benchmarkId = Nothing,
      _datasetName = "mirror-image",
      _inputTypes = [GList GInt, GList GInt],
      _outputType = GBool,
      _fitnessMetric = boolError,
      _testCases = 1000,
      _trainCases = 100,
      _relevantTypes = S.fromList $ (allowUnaryLambdas <> allowList) $ allowPairs [GInt, GBool],
      _customOutputParser = Nothing,
      _allowedConstants =
        M.fromList
          [(GBool, pure . BoolLit <$> [True, False])]
    }
