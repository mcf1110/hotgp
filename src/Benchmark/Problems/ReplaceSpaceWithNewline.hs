module Benchmark.Problems.ReplaceSpaceWithNewline where

import Benchmark.Core (Benchmark (..))
import Benchmark.Helpers (allowList, allowPairs, allowUnaryLambdas)
import Benchmark.Metrics (floatError, intError, levenshteinDistance)
import Benchmark.Parse (OutputParser, parseJsonLit, (^.))
import Control.Monad (replicateM)
import Data.HashMap.Strict ((!?))
import qualified Data.Map as M
import Data.Monoid (Sum)
import qualified Data.Set as S
import Evolution (St, random, randomR)
import Grammar
import Pretty

replaceSpaceWithNewline :: Benchmark (Sum Integer)
replaceSpaceWithNewline =
  MkBenchmark
    { _benchmarkId = Nothing,
      _datasetName = "replace-space-with-newline",
      _inputTypes = [GList GChar],
      _outputType = GPair GInt (GList GChar),
      _fitnessMetric = fitnessMetric,
      _testCases = 1000,
      _trainCases = 100,
      _relevantTypes =
        S.fromList $
          allowUnaryLambdas baseTypes <> allowPairs [GInt, GList GChar],
      _customOutputParser = Just parseOutput,
      _allowedConstants =
        M.fromList
          [ (GChar, (pure . CharLit <$> [' ', '\n']) <> [CharLit <$> randomR ('!', '~')]),
            (GList GChar, [stLit <$> stringErc])
          ]
    }
  where
    baseTypes = [GBool, GChar, GInt, GList GChar]

parseOutput :: OutputParser
parseOutput obj = pairLit (parseJsonLit GInt (obj ^. "output2"), parseJsonLit (GList GChar) (obj ^. "output1"))

fitnessMetric :: Lit -> Lit -> Sum Integer
fitnessMetric (PairLit a b) (PairLit x y) = intError a x <> levenshteinDistance b y
fitnessMetric a b = error $ "Cannot apply fitness metric to " <> pretty a <> " and " <> pretty b

stringErc :: St String
stringErc = do
  len <- randomR (0, 20 :: Int)
  replicateM len $ do
    space <- (< 0.2) <$> randomR (0, 1.0 :: Float)
    if space then return ' ' else randomR ('a', 'z')
