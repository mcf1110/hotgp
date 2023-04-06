module Benchmark.Problems.DoubleLetters where

import Benchmark.Core
import Benchmark.Helpers
import Benchmark.Metrics (levenshteinDistance)
import qualified Data.Map as M
import Data.Monoid (Sum (Sum))
import qualified Data.Set as S
import Grammar

doubleLetters :: Benchmark (Sum Integer)
doubleLetters =
  MkBenchmark
    { _benchmarkId = Nothing,
      _datasetName = "double-letters",
      _inputTypes = [GList GChar],
      _outputType = GList GChar,
      _fitnessMetric = levenshteinDistance,
      _testCases = 1000,
      _trainCases = 100,
      _relevantTypes = S.fromList $ allowUnaryLambdas [GChar, GList GChar, GList (GList GChar), GBool],
      _customOutputParser = Nothing,
      _allowedConstants =
        M.fromList
          [ (GChar, [pure $ CharLit '!'])
          ]
    }
