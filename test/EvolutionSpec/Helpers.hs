module EvolutionSpec.Helpers where

import Benchmark
import Benchmark.BenchmarkToConfig
import Benchmark.Core
import Benchmark.Helpers
import qualified Control.Monad.State.Strict as S
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust)
import qualified Data.Set as S
import Evolution
import Grammar
import Helpers
import Pretty
import qualified System.Random as R
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)
import qualified Test.Tasty.QuickCheck as QC

makeTestConfig :: FunctionType -> Depth -> Int -> Config Int
makeTestConfig fType maxDepth sz = update $ makeConfig undefined bench
  where
    update :: Config (Result ()) -> Config Int
    update config =
      MkConfig
        { _programType = fType,
          _fullTable = _fullTable config,
          _growTable = _growTable config,
          _maxTreeDepth = maxDepth,
          _maxMutationTreeDepth = maxDepth,
          _maxInitialDepth = maxDepth,
          _popSize = sz,
          _individualsPerStep = 3,
          _nEvaluations = 3,
          _fitnessFunction = const 3,
          _parentScalar = _parentScalar config,
          _crossoverRate = _crossoverRate config
        }
    bench :: Benchmark ()
    bench =
      MkBenchmark
        { _benchmarkId = Nothing,
          _datasetName = "test",
          _inputTypes = _argTypes fType,
          _outputType = _outType fType,
          _fitnessMetric = undefined,
          _testCases = 0,
          _trainCases = 0,
          _relevantTypes = S.fromList $ [GInt, GFloat, GChar, GBool] <> _argTypes fType <> [_outType fType],
          _customOutputParser = Nothing,
          _allowedConstants =
            M.fromList
              [ (GInt, [return $ IntLit 42]),
                (GFloat, [return $ FloatLit 42.0]),
                (GChar, [return $ CharLit 'm']),
                (GBool, [return $ BoolLit True])
              ]
        }

withRandomSeed :: St a -> QC.Gen a
withRandomSeed stateful = S.evalState stateful . R.mkStdGen <$> QC.arbitrary

instance QC.Arbitrary FunctionType where
  arbitrary = do
    outType <- arbitraryType
    argTypes <- QC.listOf arbitraryType
    return (argTypes ->> outType)

arbitraryTree :: (Config Int -> Depth -> OutputType -> St Tree) -> QC.Gen (Depth, TypedTree)
arbitraryTree method = do
  depth <- QC.chooseInt (1, 10)
  fType <- QC.arbitrary
  (\t -> (depth, MkTypedTree t fType)) <$> withRandomSeed (method (makeTestConfig fType depth 10) depth (_outType fType))

arbitraryRampedOfType :: FunctionType -> QC.Gen [(Depth, TypedTree)]
arbitraryRampedOfType fType = snd <$> arbitraryRampedOfTypeWithConfig fType

arbitraryRampedOfTypeWithConfig :: FunctionType -> QC.Gen (Config Int, [(Depth, TypedTree)])
arbitraryRampedOfTypeWithConfig fType = do
  maxDepth <- QC.chooseInt (4, 10)
  size <- QC.getSize
  let cfg = makeTestConfig fType maxDepth size
  trees <- withRandomSeed $ ramped cfg
  return (cfg, (\t -> (maxDepth, MkTypedTree t fType)) <$> trees)

arbitraryRamped :: QC.Gen [(Depth, TypedTree)]
arbitraryRamped = QC.arbitrary >>= arbitraryRampedOfType

runTypeCheck :: TypedTree -> QC.Property
runTypeCheck tt = QC.counterexample ce $ isJust $ typeCheck (_type tt) (_tree tt)
  where
    ce =
      pretty (_tree tt)
        <> " does not have type "
        <> pretty (_type tt)

testMaxDepth :: Depth -> TypedTree -> QC.Property
testMaxDepth desiredMaxDepth typedTree = QC.counterexample ce (d <= desiredMaxDepth)
  where
    t = _tree typedTree
    d = getHeight t
    ce =
      "Tree `"
        <> pretty t
        <> "` has depth of "
        <> show d
        <> " but should have it at max "
        <> show desiredMaxDepth

validTypedTree :: (Depth, TypedTree) -> QC.Property
validTypedTree (depth, tt) = testMaxDepth depth tt QC..&&. runTypeCheck tt
