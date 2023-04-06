module Benchmark.Problems.VectorAverage (vectorAverage) where

import Benchmark.Core (Benchmark (..))
import Benchmark.Metrics (floatError)
import Data.Align (alignWith)
import qualified Data.Map as M
import Data.Monoid (Sum (Sum))
import qualified Data.Set as S
import Data.These (These (That, These, This))
import Evolution (randomR)
import Grammar

vectorAverage :: Benchmark (Sum Float)
vectorAverage =
  MkBenchmark
    { _benchmarkId = Nothing,
      _datasetName = "vector-average",
      _inputTypes = [GList GFloat],
      _outputType = GFloat,
      _fitnessMetric = floatError,
      _testCases = 1000,
      _trainCases = 100,
      _relevantTypes = S.fromList [GList GInt, GInt, GFloat, GBool],
      _customOutputParser = Nothing,
      _allowedConstants =
        M.fromList [(GInt, [return $ IntLit 0]), (GFloat, [return $ FloatLit 0])]
    }
