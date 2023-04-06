module EvolutionSpec.AtPointSpec where

import Control.Monad (replicateM)
import Control.Monad.State.Strict (evalState)
import Data.Bifunctor (Bifunctor (first))
import Data.List
import Evolution
import EvolutionSpec.Helpers
import Grammar
import Pretty
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

tests :: [TestTree]
tests =
  [ testGroup "At Point" atPointTests,
    atRandomPointTest
  ]

exprTree :: Tree
exprTree = AddInt <| [iLitT 5, iLitT 7]

exprTree2 :: Tree
exprTree2 = MultInt <| [AddInt <| [iLitT 1, iLitT 2], iLitT 3]

replaceWith10 :: Point -> Tree -> Tree
replaceWith10 point tree =
  evalState (atPoint point (const $ return $ iLitT 10) tree) undefined

atPointTests :: [TestTree]
atPointTests =
  [ testGroup
      "Only Root"
      [testCase "Updates position 1" $ replaceWith10 1 (iLitT 5) @?= iLitT 10],
    testGroup
      "Simple add"
      [ testCase "Updates position 1" $ replaceWith10 1 exprTree @?= iLitT 10,
        testCase "Updates position 2" $
          replaceWith10 2 exprTree @?= AddInt <| [iLitT 10, iLitT 7],
        testCase "Updates position 3" $
          replaceWith10 3 exprTree @?= AddInt <| [iLitT 5, iLitT 10]
      ],
    testGroup
      "Mult and Add"
      [ testCase "Updates position 1" $ replaceWith10 1 exprTree2 @?= iLitT 10,
        testCase "Updates position 2" $
          replaceWith10 2 exprTree2 @?= MultInt <| [iLitT 10, iLitT 3],
        testCase "Updates position 3" $
          replaceWith10 3 exprTree2
            @?= MultInt <| [AddInt <| [iLitT 10, iLitT 2], iLitT 3],
        testCase "Updates position 4" $
          replaceWith10 4 exprTree2
            @?= MultInt <| [AddInt <| [iLitT 1, iLitT 10], iLitT 3],
        testCase "Updates position 5" $
          replaceWith10 5 exprTree2
            @?= MultInt <| [AddInt <| [iLitT 1, iLitT 2], iLitT 10]
      ]
  ]

possibleTrees :: [Tree]
possibleTrees =
  [ iLitT 10,
    MultInt <| [iLitT 10, iLitT 3],
    MultInt <| [AddInt <| [iLitT 10, iLitT 2], iLitT 3],
    MultInt <| [AddInt <| [iLitT 1, iLitT 10], iLitT 3],
    MultInt <| [AddInt <| [iLitT 1, iLitT 2], iLitT 10]
  ]

containSameElements :: Eq a => [a] -> [a] -> Bool
containSameElements l1 l2 = all (`elem` l1) l2 && all (`elem` l2) l1

countElements :: Eq a => [a] -> [(a, Int)]
countElements xs = map (\x -> (x, length . filter (== x) $ xs)) $ nub xs

atRandomPointTest :: TestTree
atRandomPointTest = QC.testProperty "Nodes are picked uniformly at random" $
  do
    trees <-
      withRandomSeed $
        replicateM 1000 $
          atRandomPoint (const $ return $ iLitT 10) exprTree2
    let allAreRepresented =
          QC.counterexample (pretty . nub $ trees) $
            containSameElements trees possibleTrees
        counts = countElements trees
        allAreInAcceptableRange =
          QC.counterexample
            (show $ first pretty <$> counts)
            $ all (\x -> x > 150 && x < 250) $
              snd <$> counts
    return $ allAreRepresented QC..&&. allAreInAcceptableRange
