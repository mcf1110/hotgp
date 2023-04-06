module Benchmark.BenchmarkToConfig where

import Benchmark.Core
import Benchmark.Dataset
import Benchmark.Helpers
import qualified Data.Map as M
import Data.Maybe (isJust)
import qualified Data.Set as S
import Evolution
import Grammar

makeConfig :: (Ord a, Monoid a) => Dataset -> Benchmark a -> Config (Result a)
makeConfig trainSet bench =
  MkConfig
    { _programType = MkFunctionType {_argTypes = _inputTypes bench, _outType = _outputType bench},
      _maxTreeDepth = treeDepth,
      _maxMutationTreeDepth = treeDepth,
      _maxInitialDepth = treeDepth,
      _popSize = popSize,
      _individualsPerStep = 2,
      _nEvaluations = nGens * popSize,
      _fitnessFunction = fitness,
      _parentScalar = 0.9993,
      _crossoverRate = 0.5,
      _fullTable = buildTable termsAndOps treeDepth True,
      _growTable = buildTable termsAndOps treeDepth False
    }
  where
    popSize = 1000
    nGens = 300
    treeDepth = 15
    fitness tree = mconcat $ (\(x, y) -> _fitnessMetric bench y <$> maybeToResult (evalTree x tree)) <$> trainSet

    termsAndOps :: TermsAndOps
    termsAndOps =
      MkTermsAndOps
        { _literals = _allowedConstants bench,
          _arguments = typesToArgMap (_inputTypes bench),
          _operations = operationsFromTypes (S.toList $ _relevantTypes bench)
        }

operationsFromTypes :: [GType] -> M.Map OutputType [Operation]
operationsFromTypes relevantTypes = M.fromList [(t, findOps t) | t <- relevantTypes]
  where
    ops = [(minBound :: Operation) .. maxBound]
    findOps t = filter (opMatches t) ops
    opMatches :: GType -> Operation -> Bool
    opMatches gType op = isJust $ unifyTypes gType $ opOutput op
