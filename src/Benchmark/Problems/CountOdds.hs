module Benchmark.Problems.CountOdds (countOdds) where

import Benchmark.Core (Benchmark (..))
import Benchmark.Helpers
import Benchmark.Metrics
import Data.Align (alignWith)
import qualified Data.Map as M
import Data.Monoid (Sum (Sum))
import qualified Data.Set as S
import Evolution (randomR)
import Grammar

countOdds :: Benchmark (Sum Integer)
countOdds =
  MkBenchmark
    { _benchmarkId = Nothing,
      _datasetName = "count-odds",
      _inputTypes = [GList GInt],
      _outputType = GInt,
      _fitnessMetric = intError,
      _testCases = 2000,
      _trainCases = 200,
      _relevantTypes = S.fromList $ allowUnaryLambdas [GList GInt, GInt, GBool],
      _customOutputParser = Nothing,
      _allowedConstants =
        M.fromList
          [ ( GInt,
              map
                (IntLit <$>)
                [ return 0,
                  return 1,
                  return 2,
                  randomR (-1000, 1000)
                ]
            )
          ]
    }
