{-# LANGUAGE GADTs #-}

module Benchmark.Problems (runProblem, allProblemIds, searchCxRateProblem, getProblem, Problem (MkProblem)) where

import Benchmark.Core
import Benchmark.Problems.CollatzNumbers (collatzNumbers)
import Benchmark.Problems.CompareStringLengths (compareStringLengths)
import Benchmark.Problems.CountOdds (countOdds)
import Benchmark.Problems.Digits (digits)
import Benchmark.Problems.DoubleLetters (doubleLetters)
import Benchmark.Problems.EvenSquares (evenSquares)
import Benchmark.Problems.ForLoopIndex (forLoopIndex)
import Benchmark.Problems.Grade (grade)
import Benchmark.Problems.LastIndexOfZero (lastIndexOfZero)
import Benchmark.Problems.Median (median)
import Benchmark.Problems.MirrorImage (mirrorImage)
import Benchmark.Problems.NegativeToZero (negativeToZero)
import Benchmark.Problems.NumberIO (numberIO)
import Benchmark.Problems.ReplaceSpaceWithNewline (replaceSpaceWithNewline)
import Benchmark.Problems.ReplaceSpaceWithNewlineFst (replaceSpaceWithNewlineFst)
import Benchmark.Problems.ReplaceSpaceWithNewlineSnd (replaceSpaceWithNewlineSnd)
import Benchmark.Problems.SmallOrLarge (smallOrLarge)
import Benchmark.Problems.Smallest (smallest)
import Benchmark.Problems.StringDifferences (stringDifferences)
import Benchmark.Problems.StringLengthsBackwards (stringLengthsBackwards)
import Benchmark.Problems.SumOfSquares (sumOfSquares)
import Benchmark.Problems.Syllables (syllables)
import Benchmark.Problems.VectorAverage (vectorAverage)
import Benchmark.Problems.VectorsSummed (vectorsSummed)
import Benchmark.Problems.WallisPi (wallisPi)
import Benchmark.Run (runBenchmark)
import Benchmark.RunSearch (searchCxRate)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Evolution (runAndLog)
import Evolution.Fitness (Fitness)

data Problem where
  MkProblem :: (Fitness a, Monoid a, Show a) => Benchmark a -> Problem

allProblems :: [Problem]
allProblems =
  [ --MkProblem collatzNumbers,
    MkProblem compareStringLengths,
    MkProblem countOdds,
    MkProblem digits,
    MkProblem doubleLetters,
    MkProblem evenSquares,
    MkProblem forLoopIndex,
    MkProblem grade,
    MkProblem lastIndexOfZero,
    MkProblem median,
    MkProblem mirrorImage,
    MkProblem negativeToZero,
    MkProblem numberIO,
    MkProblem replaceSpaceWithNewline,
    MkProblem replaceSpaceWithNewlineFst,
    MkProblem replaceSpaceWithNewlineSnd,
    MkProblem smallest,
    MkProblem smallOrLarge,
    MkProblem stringDifferences,
    MkProblem stringLengthsBackwards,
    MkProblem sumOfSquares,
    MkProblem syllables,
    MkProblem vectorAverage,
    MkProblem vectorsSummed,
    MkProblem wallisPi
  ]

getProblem :: String -> Maybe Problem
getProblem name = find byId allProblems
  where
    byId :: Problem -> Bool
    byId = (name ==) . problemId

problemId :: Problem -> String
problemId (MkProblem b) = getBenchmarkId b

allProblemIds :: [String]
allProblemIds = problemId <$> allProblems

runProblem :: FilePath -> Int -> String -> IO ()
runProblem workDir seed problemName = case getProblem problemName of
  Nothing -> error "Unknown benchmark name"
  Just (MkProblem bench) -> do
    runBenchmark workDir seed bench

searchCxRateProblem :: FilePath -> Int -> Int -> String -> IO ()
searchCxRateProblem workDir foldNumber cxRate problemName = case getProblem problemName of
  Nothing -> error "Unknown benchmark name"
  Just (MkProblem bench) -> do
    searchCxRate workDir foldNumber cxRate bench
