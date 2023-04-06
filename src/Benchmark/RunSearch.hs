{-# LANGUAGE ScopedTypeVariables #-}

module Benchmark.RunSearch where

import Benchmark.BenchmarkToConfig
import Benchmark.Core
import Benchmark.Dataset
import Benchmark.Download (csvDirs, downloadBenchmark)
import Benchmark.Helpers
import Benchmark.Log
import Control.Monad (unless, when)
import Control.Monad.State.Strict (evalState, evalStateT)
import qualified Data.ByteString.Lazy as B
import Data.Either (fromRight)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.SortedList as SL
import Data.Time
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.Vector as V
import Evolution
import Evolution.Fitness (Fitness)
import Grammar
import Pretty
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, removeFile)
import System.IO
import System.Random (mkStdGen)

searchCxRate :: (Fitness a, Monoid a, Show a) => FilePath -> Int -> Int -> Benchmark a -> IO ()
searchCxRate workDir foldNumber cxRate benchmark = do
  hSetBuffering stdout $ BlockBuffering Nothing
  putStrLn $ "Testing " <> getBenchmarkId benchmark <> " with cxRate=" <> show cxRate <> "%, on fold " <> show foldNumber
  identifier <- logFileIdentifier foldNumber cxRate benchmark
  (logFileHandle, finalResultFile, _) <- prepareLogFiles identifier workDir benchmark
  (trainSet, testSet) <- loadTrainAndValidationSet foldNumber workDir benchmark

  let cfg = (makeConfig trainSet benchmark) {_crossoverRate = fromIntegral cxRate / 100}
      checkpoint = checkpointFilename workDir foldNumber cxRate benchmark
      logToFile cache time evals pop = do
        let (row, newCache) = logPop cache testSet time evals pop
        hPutStrLn logFileHandle row
        when (evals `mod` 100 == 0) $ hFlush logFileHandle
        return newCache

  (finalPop, _) <- evalStateT (runAndLog cfg logToFile checkpoint) $ mkStdGen 0

  hClose logFileHandle
  writeFile finalResultFile $ logBest testSet (_indTree $ head $ SL.fromSortedList finalPop)

-- shouldRemoveCheck <- doesFileExist checkpoint
-- when shouldRemoveCheck $ removeFile checkpoint

logFileIdentifier :: Int -> Int -> Benchmark a -> IO String
logFileIdentifier foldNumber cxRate benchmark = do
  time <- getCurrentTime
  return $ iso8601Show time <> "_f" <> show foldNumber <> "_cx" <> show cxRate <> "_" <> getBenchmarkId benchmark

checkpointFilename :: FilePath -> Int -> Int -> Benchmark a -> FilePath
checkpointFilename _ foldNumber cxRate benchmark = "./checkpoint/" <> "f" <> show foldNumber <> "_cx" <> show cxRate <> "_" <> getBenchmarkId benchmark <> ".checkpoint"
