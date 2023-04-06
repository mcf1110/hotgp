module Benchmark.Problems.NumberIO (numberIO) where

import Benchmark.Core (Benchmark (..))
import Benchmark.Metrics (floatError)
import qualified Data.Map as M
import Data.Monoid (Sum)
import qualified Data.Set as S
import Evolution (randomR)
import Grammar

numberIO :: Benchmark (Sum Float)
numberIO =
  MkBenchmark
    { _benchmarkId = Nothing,
      _datasetName = "number-io",
      _inputTypes = [GFloat, GInt],
      _outputType = GFloat,
      _fitnessMetric = floatError,
      _testCases = 1000,
      _trainCases = 250,
      _relevantTypes = S.fromList [GFloat, GInt],
      _customOutputParser = Nothing,
      _allowedConstants =
        M.fromList
          [ (GFloat, [FloatLit <$> randomR (-100, 100)]),
            (GInt, [IntLit <$> randomR (-100, 100)])
          ]
    }
