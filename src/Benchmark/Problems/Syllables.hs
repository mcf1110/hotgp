module Benchmark.Problems.Syllables (syllables) where

import Benchmark.Core (Benchmark (..))
import Benchmark.Helpers
import Benchmark.Metrics
import Control.Monad
import Data.Align (alignWith)
import qualified Data.Map as M
import Data.Monoid (Sum (Sum))
import qualified Data.Set as S
import Evolution (randomR, sample)
import Evolution.Core (St)
import Grammar

syllables :: Benchmark (Sum Integer)
syllables =
  MkBenchmark
    { _benchmarkId = Nothing,
      _datasetName = "syllables",
      _inputTypes = [GList GChar],
      _outputType = GList GChar,
      _fitnessMetric = levenshteinDistance,
      _testCases = 2000,
      _trainCases = 200,
      _relevantTypes = S.fromList $ allowUnaryLambdas [GList GChar, GChar, GInt, GBool],
      _customOutputParser = Nothing,
      _allowedConstants =
        M.fromList
          [ (GInt, [IntLit <$> randomR (0, 100)]),
            ( GList GChar,
              ( pure . stLit
                  <$> [ "The number of syllables is ",
                        vowels
                      ]
              )
                <> [stLit <$> stringErc]
            ),
            (GChar, (pure . CharLit <$> vowels) <> [CharLit <$> randomR ('!', '~')])
          ]
    }

vowels :: [Char]
vowels = "aeiouy"

nonVowels :: [Char]
nonVowels = filter (`notElem` vowels) ['a' .. 'z'] <> " 123456789"

stringErc :: St String
stringErc = do
  len <- randomR (0, 20 :: Int)
  replicateM len $ do
    isVowel <- (< 0.2) <$> randomR (0, 1.0 :: Float)
    sample $ if isVowel then vowels else nonVowels
