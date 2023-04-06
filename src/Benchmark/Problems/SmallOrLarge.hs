{-# LANGUAGE NumericUnderscores #-}

module Benchmark.Problems.SmallOrLarge where

import Benchmark.Core
import Benchmark.Helpers
  ( allowList,
    allowPairs,
  )
import Benchmark.Metrics (levenshteinDistance)
import qualified Data.Map as M
import Data.Monoid (Sum (Sum))
import qualified Data.Set as S
import Evolution (randomR, sample)
import Grammar

smallOrLarge :: Benchmark (Sum Integer)
smallOrLarge =
  MkBenchmark
    { _benchmarkId = Nothing,
      _datasetName = "small-or-large",
      _inputTypes = [GInt],
      _outputType = GList GChar,
      _fitnessMetric = levenshteinDistance,
      _testCases = 1000,
      _trainCases = 100,
      _relevantTypes = S.fromList [GInt, GBool, GList GChar],
      _customOutputParser = Nothing,
      _allowedConstants =
        M.fromList
          [ (GInt, [intRng]),
            (GList GChar, pure . stLit <$> ["small", "large"])
          ]
    }
  where
    intRng = IntLit <$> randomR (-10_000, 10_000)
