module Benchmark.Core where

import Benchmark.Parse (OutputParser)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Evolution
import Grammar

data Benchmark a = MkBenchmark
  { -- | Name of the csv file to be downloaded from github
    _datasetName :: String,
    -- | How the CLI will identify this benchmark. Defaults to _datasetName
    _benchmarkId :: Maybe String,
    -- | Types for parsing the arguments from the csv
    _inputTypes :: ProgramArgTypes,
    -- | Types for parsing the expected result from the csv
    _outputType :: OutputType,
    -- | How can we compare two answers?
    _fitnessMetric :: Lit -> Lit -> a,
    -- | How many test cases should we use in total?
    _testCases :: Int,
    -- | How many train cases should we use in total?
    _trainCases :: Int,
    -- | Types allowed to exist in the context of the program.
    _relevantTypes :: Set GType,
    -- | Which constants are we going to allow for each of the types?
    _allowedConstants :: Map GType [St Lit],
    -- | Should this problem parse its output differently?
    _customOutputParser :: Maybe OutputParser
  }

type StatsCache = (Tree, (Float, Maybe Float))

getBenchmarkId :: Benchmark a -> String
getBenchmarkId b = fromMaybe (_datasetName b) (_benchmarkId b)