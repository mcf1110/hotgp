module Benchmark.Problems.StringLengthsBackwards (stringLengthsBackwards) where

import Benchmark.Core
import Benchmark.Helpers
import Benchmark.Metrics (levenshteinDistance)
import qualified Data.Map as M
import Data.Monoid (Sum (Sum))
import qualified Data.Set as S
import Evolution (randomR)
import Grammar

stringLengthsBackwards :: Benchmark (Sum Integer)
stringLengthsBackwards =
  MkBenchmark
    { _benchmarkId = Nothing,
      _datasetName = "string-lengths-backwards",
      _inputTypes = [GList (GList GChar)],
      _outputType = GList GChar,
      _fitnessMetric = levenshteinDistance,
      _testCases = 1000,
      _trainCases = 100,
      _relevantTypes = S.fromList $ (allowUnaryLambdas <> allowList) baseTypes,
      _customOutputParser = Nothing,
      _allowedConstants = M.fromList [(GInt, [intRng])]
    }
  where
    baseTypes = [GInt, GBool, GList GChar]
    intRng = IntLit <$> randomR (-100, 100)
