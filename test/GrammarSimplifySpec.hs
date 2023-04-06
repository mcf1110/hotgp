{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}

module GrammarSimplifySpec where

import Data.Maybe (isJust)
import Evolution (full)
import EvolutionSpec.Helpers
import Grammar
import Grammar.Simplify (simplifyTree)
import Helpers
import Pretty (Pretty (pretty))
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

tests :: [TestTree]
tests =
  [ localOption (QC.QuickCheckTests 1000) $
      -- localOption (mkTimeout 1_000_000) $
      QC.testProperty
        "Running simplifications does not break evaluation"
        testDoesNotBreak,
    localOption (QC.QuickCheckTests 1000) $
      QC.testProperty
        "Running simplifications does not grow tree"
        testDoesNotGrow,
    testGroup "Unit tests" unitTests
  ]

newtype FullTree = FullTree ([Lit], Tree) deriving (Show)

instance QC.Arbitrary FullTree where
  arbitrary :: QC.Gen FullTree
  arbitrary = do
    (depth, typedTree) <- arbitraryTree full
    lits <- sequence $ genLitOfType <$> _argTypes (_type typedTree)
    return $ FullTree (lits, _tree typedTree)

  shrink :: FullTree -> [FullTree]
  shrink (FullTree (_, Leaf {})) = []
  shrink (FullTree (lits, Node {_operation = op, _args = subtrees})) =
    (mkFullTree <$> (simplifiedChildren ++ subtrees)) ++ (subtrees >>= QC.shrink . mkFullTree)
    where
      mkFullTree = FullTree . (lits,)
      simplifiedChildren =
        let simpleTrees = simplifyTree <$> subtrees
         in [op <| simpleTrees | simpleTrees /= subtrees]

(=~=) :: Maybe Lit -> Maybe Lit -> QC.Property
Just (FloatLit a) =~= Just (FloatLit b) | isInfinite a, isInfinite b = QC.property True
Just (FloatLit a) =~= Just (FloatLit b) | isNaN a, isNaN b = QC.property True
Just (FloatLit a) =~= Just (FloatLit b) | isNaN a || isNaN b || isInfinite a || isInfinite b = QC.discard
Just (FloatLit a) =~= Just (FloatLit b) = QC.counterexample (show a ++ " and " ++ show b ++ " are not close floats") $ abs (a - b) < 1e-2
Just (PairLit a1 a2) =~= Just (PairLit b1 b2) = Just a1 =~= Just b1 QC..&&. Just a2 =~= Just b2
Just (ListLit x xs) =~= Just (ListLit y ys) | x == y = QC.conjoin $ zipWith (=~=) (Just <$> xs) (Just <$> ys)
a =~= b = a QC.=== b

testDoesNotBreak :: FullTree -> QC.Gen QC.Property
testDoesNotBreak (FullTree (args, tree)) = do
  let simpleTree = simplifyTree tree
      evaluated = evalTree args tree
  return $
    isJust evaluated
      QC.==> QC.counterexample
        ( unlines [pretty tree, "evaluates to", show evaluated, "=========", pretty simpleTree, "evaluates to", show $ evalTree args simpleTree]
        )
      $ evaluated =~= evalTree args simpleTree

testDoesNotGrow :: FullTree -> QC.Gen QC.Property
testDoesNotGrow (FullTree (args, tree)) = do
  let simpleTree = simplifyTree tree
      simpleNodes = getNodeCount simpleTree
      nodes = getNodeCount tree
  return $
    QC.label (showAsRange simpleNodes nodes <> " of original size") $
      QC.counterexample (unlines [pretty tree, "=========", pretty simpleTree]) $
        simpleNodes <= nodes
  where
    showAsRange a b
      | a == b = "100%"
      | otherwise = let c = 10 * a `div` b in concat ["(", show $ c * 10, "%-", show $ (c + 1) * 10, "%("]

simplifiesTo :: Tree -> Tree -> Assertion
simplifiesTo subject expected = simplifyTree subject @?= expected

unitTests :: [TestTree]
unitTests =
  [ testGroup
      "Dead code elimination"
      [ testCase "If True" $ (If <| [bLitT True, arg 0, arg 1]) `simplifiesTo` arg 0,
        testCase "If False" $ (If <| [bLitT False, arg 0, arg 1]) `simplifiesTo` arg 1,
        testCase "Fst" $ (Fst <| [ToPair <| [arg 0, arg 1]]) `simplifiesTo` arg 0,
        testCase "Snd" $ (Snd <| [ToPair <| [arg 0, arg 1]]) `simplifiesTo` arg 1
      ],
    testGroup
      "Skip evaluation for equal expressions"
      [ testCase "Equals" $ (EqInt <| [arg 0, arg 0]) `simplifiesTo` bLitT True,
        testCase "Less than" $ (LtInt <| [arg 0, arg 0]) `simplifiesTo` bLitT False,
        testCase "Greater than " $ (GtInt <| [arg 0, arg 0]) `simplifiesTo` bLitT False,
        testCase "Min " $ (MinInt <| [arg 0, arg 0]) `simplifiesTo` arg 0,
        testCase "Max " $ (MaxInt <| [arg 0, arg 0]) `simplifiesTo` arg 0,
        testCase "And " $ (And <| [arg 0, arg 0]) `simplifiesTo` arg 0,
        testCase "Or " $ (Or <| [arg 0, arg 0]) `simplifiesTo` arg 0,
        testCase "If " $ (If <| [arg 1, iLitT 42, iLitT 42]) `simplifiesTo` iLitT 42
      ],
    testGroup
      "Boolean algebra replacements"
      [ testCase "T || x = T" $ (Or <| [bLitT True, arg 0]) `simplifiesTo` bLitT True,
        testCase "x || T = T" $ (Or <| [arg 0, bLitT True]) `simplifiesTo` bLitT True,
        testCase "F || x = x" $ (Or <| [bLitT False, arg 0]) `simplifiesTo` arg 0,
        testCase "x || F = x" $ (Or <| [arg 0, bLitT False]) `simplifiesTo` arg 0,
        testCase "T && x = x" $ (And <| [bLitT True, arg 0]) `simplifiesTo` arg 0,
        testCase "x && T = x" $ (And <| [arg 0, bLitT True]) `simplifiesTo` arg 0,
        testCase "F && x = F" $ (And <| [bLitT False, arg 0]) `simplifiesTo` bLitT False,
        testCase "x && F = F" $ (And <| [arg 0, bLitT False]) `simplifiesTo` bLitT False,
        testCase "!x ? a : b => x ? b : a" $ (If <| [Not <| [arg 0], iLitT 0, iLitT 1]) `simplifiesTo` (If <| [arg 0, iLitT 1, iLitT 0])
      ],
    testGroup
      "Algebra replacements"
      [ testGroup
          "Addition"
          [ testCase "0 + x = x" $ (AddInt <| [iLitT 0, arg 0]) `simplifiesTo` arg 0,
            testCase "x + 0 = x" $ (AddInt <| [arg 0, iLitT 0]) `simplifiesTo` arg 0,
            testCase "0.0f + x = x" $ (AddFloat <| [fLitT 0, arg 0]) `simplifiesTo` arg 0,
            testCase "x + 0.0f = x" $ (AddFloat <| [arg 0, fLitT 0]) `simplifiesTo` arg 0,
            testGroup
              "Is Associative"
              [ testGroup
                  "Int"
                  [ testCase "A + (B + x) = (A + B) + x" $ (AddInt <| [iLitT 1, AddInt <| [iLitT 2, arg 0]]) `simplifiesTo` (AddInt <| [iLitT 3, arg 0]),
                    testCase "A + (x + B) = (A + B) + x" $ (AddInt <| [iLitT 1, AddInt <| [arg 0, iLitT 2]]) `simplifiesTo` (AddInt <| [iLitT 3, arg 0]),
                    testCase "(A + x) + B = (A + B) + x" $ (AddInt <| [AddInt <| [iLitT 2, arg 0], iLitT 1]) `simplifiesTo` (AddInt <| [iLitT 3, arg 0]),
                    testCase "(x + A) + B = (A + B) + x" $ (AddInt <| [AddInt <| [arg 0, iLitT 2], iLitT 1]) `simplifiesTo` (AddInt <| [iLitT 3, arg 0])
                  ],
                testGroup
                  "Float"
                  [ testCase "A + (B + x) = (A + B) + x" $ (AddFloat <| [fLitT 1, AddFloat <| [fLitT 2, arg 0]]) `simplifiesTo` (AddFloat <| [fLitT 3, arg 0]),
                    testCase "A + (x + B) = (A + B) + x" $ (AddFloat <| [fLitT 1, AddFloat <| [arg 0, fLitT 2]]) `simplifiesTo` (AddFloat <| [fLitT 3, arg 0]),
                    testCase "(A + x) + B = (A + B) + x" $ (AddFloat <| [AddFloat <| [fLitT 2, arg 0], fLitT 1]) `simplifiesTo` (AddFloat <| [fLitT 3, arg 0]),
                    testCase "(x + A) + B = (A + B) + x" $ (AddFloat <| [AddFloat <| [arg 0, fLitT 2], fLitT 1]) `simplifiesTo` (AddFloat <| [fLitT 3, arg 0])
                  ]
              ]
          ],
        testGroup
          "Subtraction"
          [ testCase "x - 0 = x" $ (SubInt <| [arg 0, iLitT 0]) `simplifiesTo` arg 0,
            testCase "x - 0.0f = x" $ (SubFloat <| [arg 0, fLitT 0]) `simplifiesTo` arg 0,
            testCase "x - x = 0" $ (SubInt <| [arg 0, arg 0]) `simplifiesTo` iLitT 0,
            testCase "x - x = 0.0f" $ (SubFloat <| [arg 0, arg 0]) `simplifiesTo` fLitT 0
          ],
        testGroup
          "Multiplication"
          [ testCase "1 * x = x" $ (MultInt <| [iLitT 1, arg 0]) `simplifiesTo` arg 0,
            testCase "x * 1 = x" $ (MultInt <| [arg 0, iLitT 1]) `simplifiesTo` arg 0,
            testCase "1.0f * x = x" $ (MultFloat <| [fLitT 1, arg 0]) `simplifiesTo` arg 0,
            testCase "x * 1.0f = x" $ (MultFloat <| [arg 0, fLitT 1]) `simplifiesTo` arg 0,
            testCase "0 * x = 0" $ (MultInt <| [iLitT 0, arg 0]) `simplifiesTo` iLitT 0,
            testCase "x * 0 = 0" $ (MultInt <| [arg 0, iLitT 0]) `simplifiesTo` iLitT 0,
            testCase "0.0f * x = 0" $ (MultFloat <| [fLitT 0, arg 0]) `simplifiesTo` fLitT 0,
            testCase "x * 0.0f = 0" $ (MultFloat <| [arg 0, fLitT 0]) `simplifiesTo` fLitT 0,
            testGroup
              "Is Associative"
              [ testGroup
                  "Int"
                  [ testCase "A * (B * x) = (A * B) * x" $ (MultInt <| [iLitT 3, MultInt <| [iLitT 2, arg 0]]) `simplifiesTo` (MultInt <| [iLitT 6, arg 0]),
                    testCase "A * (x * B) = (A * B) * x" $ (MultInt <| [iLitT 3, MultInt <| [arg 0, iLitT 2]]) `simplifiesTo` (MultInt <| [iLitT 6, arg 0]),
                    testCase "(A * x) * B = (A * B) * x" $ (MultInt <| [MultInt <| [iLitT 2, arg 0], iLitT 3]) `simplifiesTo` (MultInt <| [iLitT 6, arg 0]),
                    testCase "(x * A) * B = (A * B) * x" $ (MultInt <| [MultInt <| [arg 0, iLitT 2], iLitT 3]) `simplifiesTo` (MultInt <| [iLitT 6, arg 0])
                  ],
                testGroup
                  "Float"
                  [ testCase "A * (B * x) = (A * B) * x" $ (MultFloat <| [fLitT 3, MultFloat <| [fLitT 2, arg 0]]) `simplifiesTo` (MultFloat <| [fLitT 6, arg 0]),
                    testCase "A * (x * B) = (A * B) * x" $ (MultFloat <| [fLitT 3, MultFloat <| [arg 0, fLitT 2]]) `simplifiesTo` (MultFloat <| [fLitT 6, arg 0]),
                    testCase "(A * x) * B = (A * B) * x" $ (MultFloat <| [MultFloat <| [fLitT 2, arg 0], fLitT 3]) `simplifiesTo` (MultFloat <| [fLitT 6, arg 0]),
                    testCase "(x * A) * B = (A * B) * x" $ (MultFloat <| [MultFloat <| [arg 0, fLitT 2], fLitT 3]) `simplifiesTo` (MultFloat <| [fLitT 6, arg 0])
                  ]
              ]
          ],
        testGroup
          "Division"
          [ testCase "x // 1 = x" $ (DivInt <| [arg 0, iLitT 1]) `simplifiesTo` arg 0,
            testCase "x % 1 = 0" $ (ModInt <| [arg 0, iLitT 1]) `simplifiesTo` iLitT 0,
            testCase "x / 1.0f = x" $ (DivFloat <| [arg 0, fLitT 1]) `simplifiesTo` arg 0,
            testCase "x // x = 1" $ (DivInt <| [arg 0, arg 0]) `simplifiesTo` iLitT 1,
            testCase "x % x = 0" $ (ModInt <| [arg 0, arg 0]) `simplifiesTo` iLitT 0,
            testCase "x / x = 1.0f" $ (DivFloat <| [arg 0, arg 0]) `simplifiesTo` fLitT 1,
            --
            testCase "x*y // x = y" $ (DivInt <| [MultInt <| [arg 0, arg 1], arg 0]) `simplifiesTo` arg 1,
            testCase "x*y % x = 0" $ (ModInt <| [MultInt <| [arg 0, arg 1], arg 0]) `simplifiesTo` iLitT 0,
            testCase "x*y / x = y" $ (DivFloat <| [MultFloat <| [arg 0, arg 1], arg 0]) `simplifiesTo` arg 1,
            --
            testCase "y*x // x = y" $ (DivInt <| [MultInt <| [arg 1, arg 0], arg 0]) `simplifiesTo` arg 1,
            testCase "y*x % x = 0" $ (ModInt <| [MultInt <| [arg 1, arg 0], arg 0]) `simplifiesTo` iLitT 0,
            testCase "y*x / x = y" $ (DivFloat <| [MultFloat <| [arg 1, arg 0], arg 0]) `simplifiesTo` arg 1
          ]
      ],
    testGroup
      "Equals and if"
      [ testCase "1 == ($0 ? 1 : 2) => $0" $ EqInt <| [iLitT 1, If <| [arg 0, iLitT 1, iLitT 2]] `simplifiesTo` arg 0,
        testCase "2 == ($0 ? 1 : 2) => ! $0" $ EqInt <| [iLitT 2, If <| [arg 0, iLitT 1, iLitT 2]] `simplifiesTo` (Not <| [arg 0]),
        testCase "3 == ($0 ? 1 : 2) => F" $ EqInt <| [iLitT 3, If <| [arg 0, iLitT 1, iLitT 2]] `simplifiesTo` bLitT False,
        testCase "($0 ? 1 : 2) == 1 => $0" $ EqInt <| [If <| [arg 0, iLitT 1, iLitT 2], iLitT 1] `simplifiesTo` arg 0,
        testCase "($0 ? 1 : 2) == 2 => ! $0" $ EqInt <| [If <| [arg 0, iLitT 1, iLitT 2], iLitT 2] `simplifiesTo` (Not <| [arg 0]),
        testCase "($0 ? 1 : 2) == 3 => F" $ EqInt <| [If <| [arg 0, iLitT 1, iLitT 2], iLitT 3] `simplifiesTo` bLitT False
      ],
    testCase "List length ignores content" $
      ( Len
          <| [ Cons <| [arg 0, Cons <| [arg 1, Cons <| [arg 2, LeafLit $ tListLitOf GInt IntLit []]]]
             ]
      )
        `simplifiesTo` iLitT 3,
    testCase "Partial list length" $
      ( Len
          <| [Cons <| [arg 0, Cons <| [arg 1, Cons <| [arg 2, arg 3]]]]
      )
        `simplifiesTo` (AddInt <| [iLitT 3, Len <| [arg 3]]),
    testGroup
      "List identities"
      [ testCase "length . reverse is length" $(Len <| [Reverse <| [arg 0]]) `simplifiesTo` (Len <| [arg 0]),
        testCase "head . singleton is id" $ (Head <| [Singleton <| [arg 0]]) `simplifiesTo` arg 0,
        testCase "reverse . singleton is singleton" $(Reverse <| [Singleton <| [arg 0]]) `simplifiesTo` (Singleton <| [arg 0]),
        testCase "length . singleton is 1" $ (Len <| [Singleton <| [arg 0]]) `simplifiesTo` iLitT 1,
        testCase "product . singleton is id" $ (ProductInts <| [Singleton <| [arg 0]]) `simplifiesTo` arg 0,
        testCase "sum . singleton is id" $ (SumInts <| [Singleton <| [arg 0]]) `simplifiesTo` arg 0,
        testCase "reverse . reverse is id" $ (Reverse <| [Reverse <| [arg 0]]) `simplifiesTo` arg 0,
        testCase "take (length x) x is id" $ (Take <| [Len <| [arg 0], arg 0]) `simplifiesTo` arg 0,
        testCase "range x x x is [x]" $ (Range <| [arg 0, arg 0, arg 0]) `simplifiesTo` (Singleton <| [arg 0])
      ],
    testCase "Lambdas are simplified" $
      (Map <| [lambdaLitT (AddInt <| [iLitT 1, iLitT 2]) ([GInt] ->> GInt), arg 0])
        `simplifiesTo` (Map <| [lambdaLitT (iLitT 3) ([GInt] ->> GInt), arg 0]),
    testCase
      "Multiple simplifications are combined"
      $ ( If
            <| [ EqInt <| [arg 5, arg 5],
                 Fst <| [ToPair <| [arg 0, arg 1]],
                 Snd <| [ToPair <| [arg 2, arg 3]]
               ]
        )
        `simplifiesTo` arg 0
  ]
