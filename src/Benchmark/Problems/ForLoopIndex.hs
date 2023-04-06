module Benchmark.Problems.ForLoopIndex (forLoopIndex) where

import Benchmark.Core
import Benchmark.Helpers
import Benchmark.Metrics (levenshteinDistance)
import qualified Data.Map as M
import Data.Monoid (Sum (Sum))
import qualified Data.Set as S
import Evolution (randomR)
import Grammar

forLoopIndex :: Benchmark (Sum Integer)
forLoopIndex =
  MkBenchmark
    { _benchmarkId = Nothing,
      _datasetName = "for-loop-index",
      _inputTypes = [GInt, GInt, GInt],
      _outputType = GList GChar,
      _fitnessMetric = levenshteinDistance,
      _testCases = 1000,
      _trainCases = 100,
      _relevantTypes = S.fromList $ (allowUnaryLambdas <> allowList) [GInt, GChar, GList GChar],
      _customOutputParser = Nothing,
      _allowedConstants = M.empty
    }
