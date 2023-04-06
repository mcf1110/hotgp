module Benchmark.Problems.Digits (digits) where

import Benchmark.Core (Benchmark (..))
import Benchmark.Helpers
import Benchmark.Metrics
import Data.Align (alignWith)
import qualified Data.Map as M
import Data.Monoid (Sum (Sum))
import qualified Data.Set as S
import Evolution (randomR)
import Grammar

digits :: Benchmark (Sum Integer)
digits =
  MkBenchmark
    { _benchmarkId = Nothing,
      _datasetName = "digits",
      _inputTypes = [GInt],
      _outputType = GList GChar,
      _fitnessMetric = levenshteinDistance,
      _testCases = 1000,
      _trainCases = 100,
      _relevantTypes = S.fromList $ allowUnaryLambdas [GList GChar, GChar, GInt, GBool],
      _customOutputParser = Nothing,
      _allowedConstants =
        M.fromList
          [ (GInt, [IntLit <$> randomR (-10, 10)]),
            (GChar, [pure $ CharLit '\n'])
          ]
    }
