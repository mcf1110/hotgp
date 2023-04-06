module Benchmark.Problems.ReplaceSpaceWithNewlineFst where

import Benchmark.Core (Benchmark (..))
import Benchmark.Helpers (allowList, allowPairs, allowUnaryLambdas)
import Benchmark.Metrics (floatError, intError, levenshteinDistance)
import Benchmark.Parse
import Control.Monad (replicateM)
import qualified Data.Map as M
import Data.Monoid (Sum)
import qualified Data.Set as S
import Evolution (St, random, randomR)
import Grammar

replaceSpaceWithNewlineFst :: Benchmark (Sum Integer)
replaceSpaceWithNewlineFst =
  MkBenchmark
    { _benchmarkId = Just "replace-space-with-newline-fst",
      _datasetName = "replace-space-with-newline",
      _inputTypes = [GList GChar],
      _outputType = GList GChar,
      _fitnessMetric = levenshteinDistance,
      _testCases = 1000,
      _trainCases = 100,
      _relevantTypes = S.fromList $ allowUnaryLambdas baseTypes <> allowList [GChar],
      _customOutputParser = Just parseOutput,
      _allowedConstants =
        M.fromList
          [ (GChar, (pure . CharLit <$> [' ', '\n']) <> [CharLit <$> randomR ('!', '~')]),
            (GList GChar, [stLit <$> stringErc])
          ]
    }
  where
    baseTypes = [GBool, GChar, GInt]

parseOutput :: OutputParser
parseOutput obj = parseJsonLit (GList GChar) (obj ^. "output1")

stringErc :: St String
stringErc = do
  len <- randomR (0, 20 :: Int)
  replicateM len $ do
    space <- (< 0.2) <$> randomR (0, 1.0 :: Float)
    if space then return ' ' else randomR ('a', 'z')
