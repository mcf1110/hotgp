module MaxTreeDepthSpec where

import Benchmark.BenchmarkToConfig (operationsFromTypes)
import Benchmark.Helpers
import Control.Monad.State.Strict (evalState)
import qualified Data.Foldable as S
import Data.List (nub)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import qualified Data.SortedList as SL
import Evolution
import EvolutionSpec.Helpers
import Grammar (FunctionType (_argTypes), ProgramType, Terminal, getHeight)
import Grammar.Core
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

tests :: [TestTree]
tests =
  [ QC.testProperty "Evolution never exceeds max tree depth" $ QC.withMaxSuccess 10 testMaxTreeDepth
  ]

createTestConfig :: Depth -> ProgramType -> Config Int
createTestConfig depth pgType =
  MkConfig
    { _programType = pgType,
      _maxTreeDepth = depth,
      _maxMutationTreeDepth = depth,
      _maxInitialDepth = depth,
      _popSize = 10,
      _individualsPerStep = 2,
      _nEvaluations = 50,
      _fitnessFunction = const 1,
      _parentScalar = 0.98,
      _crossoverRate = 0.5,
      _fullTable = buildTable termsAndOps depth True,
      _growTable = buildTable termsAndOps depth False
    }
  where
    termsAndOps :: TermsAndOps
    termsAndOps =
      MkTermsAndOps
        { _literals =
            M.fromList
              [ (GInt, mkTerm $ IntLit 42),
                (GFloat, mkTerm $ FloatLit 42.0),
                (GChar, mkTerm $ CharLit 'm'),
                (GBool, mkTerm $ BoolLit True)
              ],
          _arguments = M.empty,
          _operations = operationsFromTypes $ allowUnaryLambdas types <> allowList types
        }
    types = allowPairs [GInt, GBool, GChar, GFloat]
    mkTerm lit = [return lit]

testMaxTreeDepth :: QC.Gen QC.Property
testMaxTreeDepth = do
  fType <- QC.arbitrary
  maxDepth <- QC.chooseInt (3, 5)
  let cfg = createTestConfig maxDepth fType
  list <- map _indTree . SL.fromSortedList <$> withRandomSeed (runEvolution cfg)
  let depths = getHeight <$> list
  return $ QC.label (show (maximum depths) <> " <= " <> show maxDepth) $ QC.conjoin $ (<= maxDepth) <$> depths
