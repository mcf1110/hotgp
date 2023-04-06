module Benchmark.Problems.EvenSquares (evenSquares) where

import Benchmark.Core
import Benchmark.Helpers
import Benchmark.Metrics
import Benchmark.Parse
import Data.Aeson (Value (String))
import Data.Align
import qualified Data.Map as M
import Data.Monoid (Sum (Sum))
import qualified Data.Set as S
import qualified Data.Text as T
import Data.These
import Evolution (randomR)
import Grammar

evenSquares :: Benchmark (Sum Integer)
evenSquares =
  MkBenchmark
    { _benchmarkId = Nothing,
      _datasetName = "even-squares",
      _inputTypes = [GInt],
      _outputType = GList GInt,
      _fitnessMetric = wrongCases,
      _testCases = 1000,
      _trainCases = 100,
      _relevantTypes = S.fromList $ (allowList <> allowUnaryLambdas) [GInt, GBool, GFloat],
      _customOutputParser = Just parseOutput,
      _allowedConstants = M.empty
    }

parseOutput :: OutputParser
parseOutput obj = tListLitOf GInt IntLit $ map (read . T.unpack) $ T.lines txt
  where
    String txt = obj ^. "output1"

wrongCases :: Lit -> Lit -> Sum Integer
wrongCases (ListLit GInt as) (ListLit GInt bs) = mconcat $ alignWith cmp as bs
  where
    cmp :: These Lit Lit -> Sum Integer
    cmp (This (IntLit a)) = Sum $ fromIntegral $ abs a
    cmp (That (IntLit b)) = Sum $ fromIntegral $ abs b
    cmp (These a b) = intError a b
    cmp _ = error "Wrong Type!"
wrongCases _ _ = error "Wrong type"
