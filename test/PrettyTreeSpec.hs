module PrettyTreeSpec where

import Grammar
import Pretty
import ProblemTrees
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testGroup "Lits" testLits,
    testGroup "Expressions" testExpressions
  ]

testLits :: [TestTree]
testLits =
  [ testCase "Int" $ pretty (iLitT 5) @?= "5",
    testCase "Float" $ pretty (fLitT 5) @?= "5.0",
    testCase "Char" $ pretty (chLitT '5') @?= "'5'",
    testCase "True" $ pretty (bLitT True) @?= "True",
    testCase "False" $ pretty (bLitT False) @?= "False",
    testCase "String" $ pretty (stLitT "Hello") @?= show "Hello",
    testCase "List" $ pretty (listLitOf IntLit [1, 2, 3]) @?= show [1, 2, 3],
    testCase "Tuple" $ pretty (pairLitT (IntLit 7, CharLit 'A')) @?= show (7, 'A')
  ]

testExpressions :: [TestTree]
testExpressions =
  [ testCase "5 + 10" $ pretty (AddInt <| [iLitT 5, iLitT 10]) @?= "5 + 10",
    testCase "5.0 + 10.0" $ pretty (AddFloat <| [fLitT 5, fLitT 10]) @?= "5.0 + 10.0",
    testCase "5 * (10 - 6)" $ pretty (MultInt <| [iLitT 5, SubInt <| [iLitT 10, iLitT 6]]) @?= "5 * (10 - 6)",
    testCase "(5 * 10) - 6" $ pretty (SubInt <| [MultInt <| [iLitT 5, iLitT 10], iLitT 6]) @?= "(5 * 10) - 6",
    testCase "not x0" $ pretty (Not <| [arg 0]) @?= "not x0",
    testCase "not (10 > x0)" $ pretty (Not <| [GtInt <| [iLitT 10, arg 0]]) @?= "not (10 > x0)",
    testCase "(\\y -> showInt y)" $ pretty (lambdaLitT (ShowInt <| [arg 0]) ([GInt] ->> GList GChar)) @?= "(\\y -> showInt y)",
    --
    -- problems
    --
    testCase "number-io" $ pretty numberIOTree @?= "(fromIntegral x0) + x1",
    testCase "small-or-large" $ pretty smallOrLargeTree @?= "if (x0 < 1000) then \"small\" else (if (x0 < 2000) then \"\" else \"large\")",
    testCase "for-loop-index" $ pretty forLoopIndexTree @?= "unlines (map (\\y -> showInt y) (range x0 x1 x2))",
    testCase "compare-string-lengths" $ pretty compareStringLengthsTree @?= "((length x0) < (length x1)) && ((length x1) < (length x2))",
    testCase "double-letters" $ pretty doubleLettersTree @?= "concat (map (\\y -> if (y == '!') then \"!!!\" else (if (isLetter y) then (y : (y : \"\")) else (y : \"\"))) x0)",
    testCase "replace-space-with-newline" $ pretty replaceSpaceWithNewlineTree @?= "(map (\\y -> if (y == ' ') then '\\n' else y) x0,length (filter (\\y -> not (y == ' ')) x0))",
    testCase "string-differences" $ pretty stringDifferencesTree @?= "filter (\\y -> not ((fst (snd y)) == (snd (snd y)))) (zip (range 0 (length x0) 1) (zip x0 x1))",
    testCase "even-squares" $ pretty evenSquaresTree @?= "filter (\\y -> 0 == (mod y 2)) (map (\\y -> y * y) (range 0 (floor (sqrt (fromIntegral x0))) 1))",
    testCase "wallis-pi" $ pretty wallisPiTree @?= "product (map (\\y -> (fromIntegral (fst y)) / (fromIntegral (snd y))) (take x0 (zip (2 : (concat (map (\\y -> y : (y : [])) (range 4 ((2 * (div x0 2)) + 2) 2)))) (concat (map (\\y -> y : (y : [])) (range 3 ((2 * (div (1 + x0) 2)) + 1) 2))))))",
    testCase "string-lengths-backwards" $ pretty stringLengthsBackwardsTree @?= "reverse (map (\\y -> length y) x0)",
    testCase "last-index-of-zero" $ pretty lastIndexOfZeroTree @?= "fst (head (reverse (filter (\\y -> 0 == (snd y)) (zip (range 0 (length x0) 1) x0))))",
    testCase "vector-average" $ pretty vectorAverageTree @?= "(sum x0) / (fromIntegral (length x0))",
    testCase "count-odds" $ pretty countOddsTree @?= "length (filter (\\y -> 1 == (mod y 2)) x0)",
    testCase "mirror-image" $ pretty mirrorImageTree @?= "(length x0) == (length (filter (\\y -> (fst y) == (snd y)) (zip x0 (reverse x1))))",
    testCase "sum-of-squares" $ pretty sumOfSquaresTree @?= "sum (map (\\y -> y * y) (range 1 x0 1))",
    testCase "vectors-summed" $ pretty vectorsSummedTree @?= "map (\\y -> (fst y) + (snd y)) (zip x0 x1)",
    testCase "negative-to-zero" $ pretty negativeToZeroTree @?= "map (\\y -> if (y < 0) then 0 else y) x0"
  ]
