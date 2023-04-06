module Benchmark.Log where

import Benchmark.Core
import Benchmark.Dataset
import Benchmark.Helpers
import Control.Applicative (liftA2)
import Control.Monad (join, unless)
import Data.List (intercalate)
import qualified Data.SortedList as SL
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format.ISO8601
import Evolution
import Evolution.Fitness
import Grammar
import Grammar.Simplify (simplifyTree)
import Pretty
import System.Directory
import System.IO

prepareLogFiles :: String -> FilePath -> Benchmark a -> IO (Handle, FilePath, FilePath)
prepareLogFiles identifier workDir benchmark = do
  let checkpointDirectory = workDir <> "checkpoint/"
      outDirectory = workDir <> "output/"
      fileNamePrefix = outDirectory <> identifier
      logFile = fileNamePrefix <> ".log.csv"
      finalResultFile = fileNamePrefix <> ".result.csv"
      jsonFile = fileNamePrefix <> ".result.json"
  createDirectoryIfMissing True outDirectory
  createDirectoryIfMissing True checkpointDirectory
  logFileHandle <- openFile logFile WriteMode
  hPutStrLn logFileHandle csvHeader
  return (logFileHandle, finalResultFile, jsonFile)

logBest :: Dataset -> Tree -> String
logBest dataset t = unlines $ header : (logTestCase <$> dataset)
  where
    header = csvJoin $ ["x" <> show n | n <- [0 .. (length (fst $ head dataset) - 1)]] <> ["y", "y_hat", "right"]
    logTestCase :: TestCase -> String
    logTestCase (x, y) = csvJoin $ (pretty <$> x) <> [pretty y, maybe "nan" pretty yHat, show $ Just y == yHat] where yHat = evalTree x t

jsonBest :: (Show a, Fitness a) => String -> Int -> Int -> Dataset -> Individual a -> String
jsonBest datasetName seed nEvals dataset ind =
  "{\n"
    <> intercalate
      ",\n"
      ( ("    " <>)
          <$> [ "datasetName" .: datasetName,
                "seed" .: seed,
                "totalEvals" .: nEvals,
                "fitness" .: logFitness (_fitness ind),
                "height" .: getHeight tree,
                "nodeCount" .: getNodeCount tree,
                "accuracy" .: accuracy,
                "nmse" .: maybe "nan" show nmse,
                "stringRepSimple" .: pretty (simplifyTree tree),
                "stringRep" .: pretty tree,
                "showTree" .: show tree
              ]
      )
    <> "\n}"
  where
    tree = _indTree ind
    (accuracy, nmse) = getAccuracyAndNmse dataset tree

(.:) :: (Show a) => String -> a -> String
key .: value = show key <> ":" <> show value

csvHeader :: String
csvHeader =
  csvJoin
    [ "time",
      "n_evals",
      "best_fitness",
      "best_depth",
      "best_node_count",
      "best_acc",
      "best_nmse",
      "best_string_rep_simple",
      "best_string_rep"
    ]

logPop :: (Show a, Fitness a) => Maybe StatsCache -> Dataset -> UTCTime -> Evaluations -> SL.SortedList (Individual a) -> (String, StatsCache)
logPop cache testSet time nEvals pop =
  ( csvJoin
      [ show time, -- time
        show nEvals, -- n_evals
        logFitness . _fitness $ best, -- best_fitness
        show . getHeight $ bestTree, -- best_depth
        show . getNodeCount $ bestTree, -- best_node_count
        show accuracy, -- best_acc
        maybe "nan" show nmse, -- best_nmse
        show . pretty . simplifyTree $ bestTree, -- best_string_rep_simple
        show . pretty $ bestTree -- best_string_rep
      ],
    newCache
  )
  where
    best = head $ SL.fromSortedList pop
    bestTree = _indTree best
    ((accuracy, nmse), newCache) = memoedGetAccuracyAndNmse cache testSet bestTree

memoedGetAccuracyAndNmse :: Maybe StatsCache -> Dataset -> Tree -> ((Float, Maybe Float), StatsCache)
memoedGetAccuracyAndNmse cache testSet tree =
  let result = getAccuracyAndNmse testSet tree
   in case cache of
        Nothing -> (result, (tree, result))
        Just cache -> memoLast (getAccuracyAndNmse testSet) cache tree

memoLast :: (Pretty a, Show a, Show b, Eq a) => (a -> b) -> (a, b) -> a -> (b, (a, b))
memoLast f t@(lastX, lastY) x
  | x == lastX = (lastY, t)
  | otherwise = let newY = f x in (newY, (x, newY))

getAccuracyAndNmse :: Dataset -> Tree -> (Float, Maybe Float)
getAccuracyAndNmse testSet tree = (accValue, nmseValue)
  where
    ysHat = (`evalTree` tree) . fst <$> testSet
    ys = snd <$> testSet
    nmseValue = join $ liftA2 (runOnNum nmse) (Just ys) (sequence ysHat)
    accValue = accuracy (Just <$> ys) ysHat

accuracy :: [Maybe Lit] -> [Maybe Lit] -> Float
accuracy ys ysHat = fromIntegral (length $ filter id $ zipWith litEq ys ysHat) / fromIntegral (length ys)
  where
    litEq :: Maybe Lit -> Maybe Lit -> Bool
    litEq (Just (FloatLit x)) (Just (FloatLit y)) = abs (x - y) < 1e-4
    litEq a b = a == b

runOnNum :: ([Float] -> [Float] -> Float) -> [Lit] -> [Lit] -> Maybe Float
runOnNum f ys ysHat = f <$> sequence (fromNum <$> ys) <*> sequence (fromNum <$> ysHat)
  where
    fromNum :: Lit -> Maybe Float
    fromNum (IntLit x) = Just $ fromIntegral x
    fromNum (FloatLit x) = Just x
    fromNum _ = Nothing

nmse :: [Float] -> [Float] -> Float
nmse ys ysHat = mse ysHat ys / var ys
  where
    mse :: [Float] -> [Float] -> Float
    mse ys ysHat = mean $ square <$> zipWith (-) ysHat ys
    mean :: [Float] -> Float
    mean x = sum x / fromIntegral (length x)
    var :: [Float] -> Float
    var x = sum $ square . (`subtract` mean x) <$> x
    square x = x * x