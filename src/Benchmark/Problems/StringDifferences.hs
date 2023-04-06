module Benchmark.Problems.StringDifferences (stringDifferences) where

import Benchmark.Core
import Benchmark.Helpers
import Benchmark.Parse (OutputParser, parseJsonLit, (^.))
import Data.Aeson (Value (String))
import Data.Align
import qualified Data.Map as M
import Data.Monoid (Sum (Sum))
import qualified Data.Set as S
import qualified Data.Text as T
import Data.These
import Evolution (randomR)
import Grammar

stringDifferences :: Benchmark (Sum Integer)
stringDifferences =
  MkBenchmark
    { _benchmarkId = Nothing,
      _datasetName = "string-differences",
      _inputTypes = [GList GChar, GList GChar],
      _outputType = GList (GPair GInt (GPair GChar GChar)),
      _fitnessMetric = wrongCases,
      _testCases = 2000,
      _trainCases = 200,
      _relevantTypes = S.fromList $ (allowList <> allowUnaryLambdas) baseTypes,
      _customOutputParser = Just parseOutput,
      _allowedConstants = M.fromList [(GInt, [intRng])]
    }
  where
    baseTypes = [GInt, GBool, GChar, GPair GChar GChar, GPair GInt (GPair GChar GChar)]
    intRng = IntLit <$> randomR (-10, 10)

parseOutput :: OutputParser
parseOutput obj = tListLitOf (GPair GInt (GPair GChar GChar)) listToTuple (map words . lines $ T.unpack txt)
  where
    String txt = obj ^. "output1"

listToTuple :: [String] -> Lit
listToTuple [i, c1, c2] = pairLit (IntLit $ read i, pairLit (CharLit $ head c1, CharLit $ head c2))
listToTuple _ = undefined

wrongCases :: Lit -> Lit -> Sum Integer
wrongCases (ListLit (GPair GInt (GPair GChar GChar)) as) (ListLit (GPair GInt (GPair GChar GChar)) bs) = mconcat $ alignWith (Sum . cmp) as bs
  where
    cmp :: These Lit Lit -> Integer
    cmp (This _) = 3
    cmp (That _) = 3
    cmp
      ( These
          (PairLit (IntLit i1) (PairLit (CharLit a1) (CharLit b1)))
          (PairLit (IntLit i2) (PairLit (CharLit a2) (CharLit b2)))
        ) = fromIntegral $ sum $ fromEnum <$> [i1 /= i2, a1 /= a2, b1 /= b2]
    cmp _ = error "Wrong type"
wrongCases _ _ = error "Wrong type"
