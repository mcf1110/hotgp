module Benchmark.Problems.WallisPi (wallisPi) where

import Benchmark.Core
import Benchmark.Helpers (allowList, allowPairs, allowUnaryLambdas)
import Benchmark.Metrics (floatError, levenshteinDistance, prettyLevenshteinDistance)
import qualified Data.Map as M
import Data.Monoid (Sum (Sum))
import qualified Data.Set as S
import Evolution (randomR)
import Grammar

wallisPi :: Benchmark (Sum Float)
wallisPi =
  MkBenchmark
    { _benchmarkId = Nothing,
      _datasetName = "wallis-pi",
      _inputTypes = [GInt],
      _outputType = GFloat,
      _fitnessMetric = fitnessMetric,
      _testCases = 50,
      _trainCases = 150,
      _relevantTypes = S.fromList $ allowUnaryLambdas $ allowList $ allowPairs [GFloat, GInt, GBool],
      _customOutputParser = Nothing,
      _allowedConstants =
        M.fromList
          [ (GFloat, [FloatLit <$> randomR (-500, 500)]),
            (GInt, [IntLit <$> randomR (-10, 10), IntLit <$> randomR (-500, 500)])
          ]
    }

fitnessMetric :: Lit -> Lit -> Sum Float
fitnessMetric x y = floatError x y <> (fromIntegral <$> prettyLevenshteinDistance x y)
