module Benchmark.Problems.Grade (grade) where

import Benchmark.Core (Benchmark (..))
import Benchmark.Helpers
import Benchmark.Metrics
import Data.Align (alignWith)
import qualified Data.Map as M
import Data.Monoid (Sum (Sum))
import qualified Data.Set as S
import Evolution (randomR)
import Grammar

grade :: Benchmark (Sum Integer)
grade =
  MkBenchmark
    { _benchmarkId = Nothing,
      _datasetName = "grade",
      _inputTypes = replicate 5 GInt,
      _outputType = GList GChar,
      _fitnessMetric = levenshteinDistance,
      _testCases = 2000,
      _trainCases = 200,
      _relevantTypes = S.fromList [GList (GList GChar), GList GChar, GInt, GBool],
      _customOutputParser = Nothing,
      _allowedConstants =
        M.fromList
          [ (GInt, [IntLit <$> randomR (0, 100)]),
            ( GList GChar,
              pure . stLit
                <$> [ "Student has a ",
                      " grade.",
                      "A",
                      "B",
                      "C",
                      "D",
                      "F"
                    ]
            )
          ]
    }
