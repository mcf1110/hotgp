module Benchmark.Problems.VectorsSummed (vectorsSummed) where

import Benchmark.Core (Benchmark (..))
import Benchmark.Helpers (allowList, allowPairs, allowUnaryLambdas)
import Data.Align (alignWith)
import qualified Data.Map as M
import Data.Monoid (Sum (Sum))
import qualified Data.Set as S
import Data.These (These (That, These, This))
import Evolution (randomR)
import Grammar

vectorsSummed :: Benchmark (Sum Integer)
vectorsSummed =
  MkBenchmark
    { _benchmarkId = Nothing,
      _datasetName = "vectors-summed",
      _inputTypes = [GList GInt, GList GInt],
      _outputType = GList GInt,
      _fitnessMetric = fitnessMetric,
      _testCases = 1500,
      _trainCases = 150,
      _relevantTypes = S.fromList $ (allowUnaryLambdas <> allowList) (allowPairs [GInt]),
      _customOutputParser = Nothing,
      _allowedConstants =
        M.fromList [(GInt, [intRng])]
    }
  where
    intRng = IntLit <$> randomR (-1000, 1000)

fitnessMetric :: Lit -> Lit -> Sum Integer
fitnessMetric (ListLit _ x) (ListLit _ y) = mconcat $ Sum . abs <$> alignWith f x y
  where
    f (These (IntLit a) (IntLit b)) = toInteger a - toInteger b
    f (This (IntLit a)) = toInteger a
    f (That (IntLit a)) = toInteger a
    f x = undefined
fitnessMetric a b = undefined
