module Benchmark.Problems.LastIndexOfZero (lastIndexOfZero) where

import Benchmark.Core
import Benchmark.Helpers
  ( allowList,
    allowPairs,
    allowUnaryLambdas,
  )
import Benchmark.Metrics (intError)
import qualified Data.Map as M
import Data.Monoid (Sum (Sum))
import qualified Data.Set as S
import Evolution (randomR)
import Grammar

lastIndexOfZero :: Benchmark (Sum Integer)
lastIndexOfZero =
  MkBenchmark
    { _benchmarkId = Nothing,
      _datasetName = "last-index-of-zero",
      _inputTypes = [GList GInt],
      _outputType = GInt,
      _fitnessMetric = intError,
      _testCases = 1000,
      _trainCases = 150,
      _relevantTypes = S.fromList $ (allowUnaryLambdas <> allowList) $ allowPairs [GBool, GInt],
      _customOutputParser = Nothing,
      _allowedConstants =
        M.fromList
          [(GInt, [return $ IntLit 0])]
    }
