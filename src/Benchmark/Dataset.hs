module Benchmark.Dataset where

import Benchmark.Core
import Benchmark.Download
import Benchmark.Parse
import qualified Control.Monad.State.Strict as S
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Either (fromRight)
import Data.HashMap.Strict ((!?))
import Data.List (intercalate)
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Vector as V
import Grammar.Core (Lit)
import System.Random (mkStdGen)

type TestCase = ([Lit], Lit)

type Dataset = [TestCase]

loadTrainAndTestSet :: FilePath -> Benchmark a -> IO (Dataset, Dataset)
loadTrainAndTestSet workDir benchmark = do
  downloadBenchmark workDir (_datasetName benchmark)
  random <- readJson workDir benchmark "-random.json"
  edge <- readJson workDir benchmark "-edge.json"
  let (trainSet, remaining) = splitAt (_trainCases benchmark) $ edge <> random
      testSet = take (_testCases benchmark) remaining
  return (trainSet, testSet)

loadTrainAndValidationSet :: Int -> FilePath -> Benchmark a -> IO (Dataset, Dataset)
loadTrainAndValidationSet foldNumber workDir benchmark = do
  downloadBenchmark workDir (_datasetName benchmark)
  random <- readJson workDir benchmark "-random.json"
  edge <- readJson workDir benchmark "-edge.json"
  let getFold _ [] = undefined
      getFold 0 xs = (head xs, tail xs)
      getFold i (x : xs) = let (el, xs') = getFold (i -1) xs in (el, x : xs')
      trainingRandomLength = _trainCases benchmark - length edge
      trainingRandom = take trainingRandomLength random
      folds = chunksOf (trainingRandomLength `div` 5) trainingRandom
      (validationSet, rest) = getFold foldNumber folds
      trainSet = edge <> concat rest
  return (trainSet, validationSet)

readJson :: FilePath -> Benchmark a -> String -> IO [([Lit], Lit)]
readJson workDir benchmark suffix = do
  fileContents <- readFile fileName
  let contents = parseJsonLine <$> lines fileContents
  return $ transform <$> contents
  where
    fileName :: FilePath
    fileName = workDir <> csvDirs <> _datasetName benchmark <> suffix

    parseOutput :: OutputParser
    parseOutput = fromMaybe (defaultOutputParser (_outputType benchmark)) (_customOutputParser benchmark)

    transform :: Object -> ([Lit], Lit)
    transform obj = (zipWith parseJsonLit (_inputTypes benchmark) xs, parseOutput obj)
      where
        xs = [obj ^. ("input" <> show i) | i <- [1 ..]]
