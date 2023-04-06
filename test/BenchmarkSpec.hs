module BenchmarkSpec where

import Benchmark.BenchmarkToConfig
import Benchmark.Helpers
import Control.Monad.State.Strict (evalState)
import Data.List (nub, sort)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Evolution (St, instantiateArgs)
import EvolutionSpec.Helpers (withRandomSeed)
import Grammar
import Grammar.Core (Operation (Singleton))
import Pretty
import System.Random (mkStdGen)
import Test.QuickCheck (Arbitrary (arbitrary))
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

intOps, floatOps, charOps, boolOps :: [Operation]
intOps = [AddInt, SubInt, MultInt, DivInt, ModInt, MaxInt, MinInt]
floatOps = [AddFloat, SubFloat, MultFloat, DivFloat, Sqrt]
charOps = []
boolOps = [And, Or, Not, If]

getOps :: GType -> [Operation]
getOps GInt = intOps
getOps GFloat = floatOps
getOps GChar = charOps
getOps GBool = boolOps
getOps _ = undefined

tests :: [TestTree]
tests =
  [ testGroup
      "Only one primitive type allowed"
      [ testCase "GInt" $ sort (snd <$> runTest GInt [GInt]) @?= sort intOps,
        testCase "GFloat" $ sort (snd <$> runTest GFloat [GFloat]) @?= sort floatOps,
        testCase "GChar" $ sort (snd <$> runTest GChar [GChar]) @?= sort charOps,
        testCase "GBool" $ sort (snd <$> runTest GBool [GBool]) @?= sort boolOps
      ],
    testGroup
      "Only two primitive types allowed"
      [ testCase "GInt + GFloat => GInt" $ sort (snd <$> runTest GInt [GInt, GFloat]) @?= sort (intOps <> [Floor]),
        testCase "GInt + GFloat => GFloat" $ sort (snd <$> runTest GFloat [GInt, GFloat]) @?= sort (floatOps <> [IntToFloat]),
        testCase "GInt + GBool  => GInt" $ sort (snd <$> runTest GInt [GInt, GBool]) @?= sort (intOps <> [If]),
        testCase "GInt + GBool  => GBool" $ sort (snd <$> runTest GBool [GInt, GBool]) @?= sort (boolOps <> [GtInt, LtInt, EqInt]),
        testCase "GChar + GBool => GChar" $ sort (snd <$> runTest GChar [GChar, GBool]) @?= sort (charOps <> [If]),
        testCase "GChar + GBool => GBool" $ sort (snd <$> runTest GBool [GChar, GBool]) @?= sort (boolOps <> [EqChar, IsLetter, IsDigit])
      ],
    testGroup
      "All four primitive types allowed"
      [ testCase "GInt" $ sort (snd <$> runTest GInt [GInt, GFloat, GBool, GChar]) @?= sort (intOps <> [Floor, If]),
        testCase "GFloat" $ sort (snd <$> runTest GFloat [GInt, GFloat, GBool, GChar]) @?= sort (floatOps <> [IntToFloat, If]),
        testCase "GBool" $ sort (snd <$> runTest GBool [GInt, GFloat, GBool, GChar]) @?= sort (boolOps <> [EqChar, IsLetter, IsDigit, GtInt, LtInt, EqInt]),
        testCase "GChar" $ sort (snd <$> runTest GChar [GInt, GFloat, GBool, GChar]) @?= sort (charOps <> [If])
      ],
    testGroup
      "Pairs of"
      [ testCase "GInt" $ sort (snd <$> runTest GInt [GInt, GPair GInt GInt]) @?= sort (intOps <> [Fst, Snd]),
        testCase "GFloat" $ sort (snd <$> runTest GFloat [GFloat, GPair GFloat GFloat]) @?= sort (floatOps <> [Fst, Snd]),
        testCase "GBool" $ sort (snd <$> runTest GBool [GBool, GPair GBool GBool]) @?= sort (boolOps <> [Fst, Snd]),
        testCase "GChar" $ sort (snd <$> runTest GChar [GChar, GPair GChar GChar]) @?= sort (charOps <> [Fst, Snd])
      ],
    testGroup
      "Heterogenous pairs returning"
      [ testCase "GInt" $ testHeterogenousPair GInt,
        testCase "GFloat" $ testHeterogenousPair GFloat,
        testCase "GChar" $ testHeterogenousPair GChar,
        testCase "GBool" $ testHeterogenousPair GBool
      ],
    testGroup
      "Lists of"
      [ testCase "GInt" $ sort (snd <$> runTest GInt [GInt, GList GInt]) @?= sort (intOps <> [Len, Head, SumInts, ProductInts]),
        testCase "GFloat" $ sort (snd <$> runTest GFloat [GFloat, GList GFloat]) @?= sort (floatOps <> [Head, SumFloats, ProductFloats]),
        testCase "GFloat" $ sort (snd <$> runTest GChar [GChar, GList GChar]) @?= sort (charOps <> [Head]),
        testCase "GFloat" $ sort (snd <$> runTest GBool [GBool, GList GBool]) @?= sort (boolOps <> [Head])
      ],
    testGroup
      "Lambdas"
      [ testCase "GInt, [GInt], (GInt -> GInt) => [GInt]" $
          sort (snd <$> runTest (GList GInt) [GInt, GList GInt, GLambda ([GInt] ->> GInt)]) @?= sort [Reverse, Singleton, Cons, Range, Take, Map],
        QC.testProperty "Expressions with lambdas are always correctly typed" $ do
          baseType <- QC.elements [GInt, GChar, GFloat]
          let relevantTypes = allowUnaryLambdas $ allowList $ allowPairs [baseType]
          outType <- QC.elements relevantTypes
          case operationsFromTypes relevantTypes M.! outType of
            [] -> QC.discard
            sts -> do
              op <- QC.elements sts
              case instantiateArgs relevantTypes outType op of
                [] -> QC.discard
                possibleArgTypes -> do
                  argTypes <- QC.elements possibleArgTypes
                  return $ QC.collect op $ isJust $ unifyTypes (GLambda $ argTypes ->> outType) (GLambda $ opType op),
        QC.testProperty "Maps are always correctly typed" $
          testLambdaOperation Map $
            \li lo i o -> QC.conjoin [i QC.=== GList li, o QC.=== GList lo],
        QC.testProperty "Filters are always correctly typed" $
          testLambdaOperation Filter $
            \li lo i o -> QC.conjoin [lo QC.=== GBool, i QC.=== o, GList li QC.=== i]
      ]
  ]

-- sampleOperation :: Operation -> [St (ArgTypes, Operation)] -> QC.Gen (ArgTypes, Operation)
-- sampleOperation desiredOp [] = QC.discard
-- sampleOperation desiredOp sts = do
--   (argTypes, op) <- withRandomSeed $ head sts
--   if op == desiredOp then return (argTypes, op) else sampleOperation desiredOp $ tail sts

runTest :: OutputType -> [GType] -> [(ArgTypes, Operation)]
runTest outType relevantTypes = do
  op <- operationsFromTypes relevantTypes M.! outType
  argTypes <- instantiateArgs relevantTypes outType op
  return (argTypes, op)

testHeterogenousPair :: GType -> Assertion
testHeterogenousPair gType = mconcat $ firsts <> seconds
  where
    firsts = (@?= sort (getOps gType <> [Fst])) <$> (map snd . runTest gType <$> [[gType, GPair gType gt] | gt <- [GInt, GFloat, GBool, GChar], gt /= gType])
    seconds = (@?= sort (getOps gType <> [Snd])) <$> (map snd . runTest gType <$> [[gType, GPair gt gType] | gt <- [GInt, GFloat, GBool, GChar], gt /= gType])

testLambdaOperation ::
  QC.Testable prop =>
  Operation ->
  (GType -> OutputType -> GType -> GType -> prop) ->
  QC.Gen QC.Property
testLambdaOperation op f = do
  baseType <- QC.elements [GInt, GChar, GFloat, GBool]
  let relevantTypes = allowUnaryLambdas (allowList $ allowPairs $ nub [baseType, GBool])
  opOutput <- QC.elements [GList baseType, GList (GPair baseType baseType)]
  case operationsFromTypes relevantTypes M.! opOutput of
    [] -> QC.discard
    sts -> do
      argTypes <- QC.elements $ instantiateArgs relevantTypes opOutput op
      return $ case argTypes of
        [GLambda (MkFunctionType [lambdaArg] lambdaOutput), opArg] ->
          QC.collect opOutput $
            QC.counterexample ("Ill-typed " <> show op <> " operation: " <> pretty (GLambda $ [GLambda ([lambdaArg] ->> lambdaOutput), lambdaOutput] ->> opOutput)) $
              f lambdaArg lambdaOutput opArg opOutput
        x -> QC.counterexample (pretty x) False