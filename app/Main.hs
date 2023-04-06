{-# LANGUAGE TupleSections #-}

module Main where

import Benchmark.Problems (allProblemIds, runProblem)
import Data.List (sort)
import Prune (prune)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import Text.Read (readMaybe)

workDir :: FilePath
workDir = "./"

main :: IO ()
main = do
  args <- getArgs
  let parsed = parseArgs args
  createDirectoryIfMissing True "./checkpoint"
  case parsed of
    Nothing -> do
      case args of
        ["prune"] -> prune
        _ -> showHelp
    Just (name, seed) -> runProblem workDir seed name

showHelp :: IO ()
showHelp =
  do
    putStrLn "Invalid args!"
    putStrLn "Usage:"
    putStrLn "\tstack run prune"
    putStrLn "\tstack run <benchmark_name> <seed_number>"
    putStrLn "\nHere are the available benchmarks:"
    putStr $ unlines $ map ('\t' :) $ sort allProblemIds

parseArgs :: [String] -> Maybe (String, Int)
parseArgs [nameString, seedString] =
  if nameString `elem` allProblemIds
    then (nameString,) <$> readMaybe seedString
    else Nothing
parseArgs _ = Nothing
