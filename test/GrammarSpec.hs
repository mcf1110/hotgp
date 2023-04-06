module GrammarSpec where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Char
import Data.List (intercalate, nub, sort)
import Grammar
import qualified GrammarSimplifySpec as SS
import qualified GrammarTypesSpec as TS
import Pretty
import ProblemTrees
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

tests :: [TestTree]
tests =
  [ testGroup
      "Solve Benchmark Problems"
      solveBenchmarkTests,
    testGroup "Types" TS.tests,
    testGroup "Failing lambdas" testFailingLambdas,
    testGroup "Simplification" SS.tests
  ]

testFailingLambdas :: [TestTree]
testFailingLambdas =
  [ QC.testProperty "Filter with failing lambda returns failing tree" testFilter
  ]
  where
    -- filter (\x -> x `div` 0 == x) arg0
    filterTree = Filter <| [lambdaLitT (EqInt <| [DivInt <| [arg 0, iLitT 0], arg 0]) ([GInt] ->> GBool), arg 0]
    testFilter = do
      args <- QC.listOf1 $ QC.arbitrary
      return $ evalTree [ListLit GInt $ map IntLit args] filterTree QC.=== Nothing

solveBenchmarkTests :: [TestTree]
solveBenchmarkTests =
  [ testNumberIO,
    testSmallOrLarge,
    testForLoopIndex,
    testCompareStringLengths,
    testDoubleLetters,
    testReplaceSpace,
    testStringDifferences,
    testEvenSquares,
    testWallisPi,
    testStringLengthsBackwards,
    testLastIndexOfZero,
    testVectorAverage,
    testCountOdds,
    testMirrorImage,
    testSumOfSquares,
    testVectorsSummed,
    testNegativeToZero,
    testGrade
  ]

compareSolutions :: [Lit] -> Maybe Lit -> Lit -> QC.Gen QC.Property
compareSolutions args Nothing expectedSolution =
  return $
    QC.counterexample
      ( "Evaluation failed for inputs: "
          <> pretty args
          <> "\n\tExpected: "
          <> show expectedSolution
      )
      False
compareSolutions args (Just treeSolution) expectedSolution =
  return $
    QC.counterexample
      ( "Inputs: "
          <> pretty args
          <> "\n\tExpected: "
          <> pretty expectedSolution
          <> "\n\tGot: "
          <> pretty treeSolution
      )
      $ treeSolution == expectedSolution

-- | 1. Number IO (Q 3.5.1):
-- Given an integer and a float, print their sum.
testNumberIO :: TestTree
testNumberIO = QC.testProperty "Number IO" $ do
  i <- QC.chooseInt (-100, 100)
  f <- QC.choose (-100.0, 100.0) :: QC.Gen Float
  let args = [IntLit i, FloatLit f]
      treeSolution = evalTree args numberIOTree
      expectedSolution = FloatLit (fromIntegral i + f)
  compareSolutions args treeSolution expectedSolution

-- | 2. Small or Large (Q 4.6.3):
-- Given an integer n, print “small” if n < 1000 and “large” if n ≥ 2000 (and nothing if 1000 ≤ n < 2000)
testSmallOrLarge :: TestTree
testSmallOrLarge = QC.testProperty "Small or Large" $ do
  i <- QC.chooseInt (-10000, 10000)
  let args = [IntLit i]
      treeSolution = evalTree args smallOrLargeTree
      expectedSolution = stLit (solution i)
  compareSolutions args treeSolution expectedSolution
  where
    solution n
      | n < 1000 = "small"
      | n >= 2000 = "large"
      | otherwise = ""

-- | 3. For Loop Index (Q 4.11.7):
-- Given 3 integer inputs start, end, and step, print the integers in the sequence
testForLoopIndex :: TestTree
testForLoopIndex = QC.testProperty "For Loop Index" $ do
  start <- QC.chooseInt (-500, 500)
  end <- QC.chooseInt (start, 500)
  step <- QC.chooseInt (1, 10)
  let args = IntLit <$> [start, end, step]
      treeSolution = evalTree args forLoopIndexTree
      expectedSolution = listLitOf CharLit (solution start end step)
  compareSolutions args treeSolution expectedSolution
  where
    solution start end step = intercalate "\n" $ show <$> [start, (start + step) .. end]

-- | 4. Compare String Lengths (Q 4.11.13):
-- Given three strings n1, n2, and n3, return true if length(n1) < length(n2) < length(n3), and false otherwise.
testCompareStringLengths :: TestTree
testCompareStringLengths = QC.testProperty "Compare String Lengths" $ do
  n1 <- QC.arbitrary
  n2 <- QC.arbitrary
  n3 <- QC.arbitrary
  let args = stLit <$> [n1, n2, n3]
      treeSolution = evalTree args compareStringLengthsTree
      expectedSolution = BoolLit (solution n1 n2 n3)
  compareSolutions args treeSolution expectedSolution
  where
    solution n1 n2 n3 = length n1 < length n2 && length n2 < length n3

-- | 5. Double Letters (P 4.1)
-- Given a string, print the string, doubling every letter character, and tripling every exclamation point.
-- All other non-alphabetic and non-exclamation characters should be printed a single time each.
testDoubleLetters :: TestTree
testDoubleLetters = QC.testProperty "Double Letters" $ do
  n1 <- (QC.arbitrary :: QC.Gen String)
  let args = stLit <$> [n1]
      treeSolution = evalTree args doubleLettersTree
      expectedSolution = stLit (solution n1)
  compareSolutions args treeSolution expectedSolution
  where
    solution n1 = n1 >>= f
    f '!' = "!!!"
    f c
      | isLetter c = [c, c]
      | otherwise = [c]

-- 6. Collatz Numbers (P 4.2):
-- Given an integer, find the number of terms in the Collatz (hailstone) sequence starting from that integer.
-- NOT IMPLEMENTED DUE TO POTENTIAL INFINITE LOOP (requires explicit recursion or iterateWhile)

-- | 7. Replace Space with Newline (P 4.3):
-- Given a string input, print the string, replacing spaces with newlines.
-- Also, return the integer count of the non-whitespace characters. The input string will not have tabs or newlines.
testReplaceSpace :: TestTree
testReplaceSpace = QC.testProperty "Replace Space with Newline" $ do
  str <- (QC.arbitrary :: QC.Gen String)
  let args = stLit <$> [str]
      treeSolution = evalTree args replaceSpaceWithNewlineTree
      expectedSolution = uncurry PairLit $ bimap stLit IntLit (solution str)
  compareSolutions args treeSolution expectedSolution
  where
    solution str = (map (\x -> if x == ' ' then '\n' else x) str, length $ filter (/= ' ') str)

-- | 8. String Differences (P 4.4):
-- Given 2 strings (without whitespace) as input, find the indices at which the strings have different characters, stopping at the end
-- of the shorter one. For each such index, print a line containing the index as well as the character in each string.
testStringDifferences :: TestTree
testStringDifferences = QC.testProperty "String Differences" $ do
  str1 <- (QC.arbitrary :: QC.Gen String)
  str2 <- (QC.arbitrary :: QC.Gen String)
  let args = stLit <$> [str1, str2]
      treeSolution = evalTree args stringDifferencesTree
      expectedSolution = tListLitOf (GPair GInt (GPair GChar GChar)) pairLit $ map (bimap IntLit (pairLit . bimap CharLit CharLit)) (solution str1 str2)
  compareSolutions args treeSolution expectedSolution
  where
    solution :: String -> String -> [(Int, (Char, Char))]
    solution s1 s2 = filter (\(_, (a, b)) -> a /= b) $ zip [0 .. (max (length s1) (length s2))] $ zip s1 s2

-- | 9. Even Squares (Q 5.4.1):
-- Given an integer n, print all of the positive even perfect squares less than n on separate lines.
testEvenSquares :: TestTree
testEvenSquares = QC.testProperty "Even Squares" $ do
  i <- QC.getPositive <$> QC.arbitrary
  let args = IntLit <$> [i]
      treeSolution = evalTree args evenSquaresTree
      expectedSolution = listLitOf IntLit $ solution i
  compareSolutions args treeSolution expectedSolution
  where
    solution :: Int -> [Int]
    solution n = filter even $ map (^ 2) [0 .. floor $ sqrt (fromIntegral n)]

-- | 10. Wallis Pi (P 6.4)):
-- John Wallis gave the following infinite product that converges to π/4.
-- Given an integer input n, compute an approximation of this product out to n terms
testWallisPi :: TestTree
testWallisPi = QC.testProperty "Wallis Pi" $ do
  i <- QC.getPositive <$> QC.arbitrary
  let args = IntLit <$> [i]
      treeSolution = evalTree args wallisPiTree
      expectedSolution = FloatLit $ solution i
  compareSolutions args treeSolution expectedSolution
  where
    solution :: Int -> Float
    solution n = product $ take n $ zipWith (/) (2 : ([4, 6 ..] >>= (\x -> [x, x]))) ([3, 5 ..] >>= (\x -> [x, x]))

-- | 11. String Lengths Backwards (Q 7.2.5):
-- Given a vector of strings, print the length of each string in the vector starting with the last and ending with the first.
testStringLengthsBackwards :: TestTree
testStringLengthsBackwards = QC.testProperty "String Lengths Backwards" $ do
  strings <- QC.listOf1 QC.arbitrary
  let args = [listLitOf stLit strings]
      treeSolution = evalTree args stringLengthsBackwardsTree
      expectedSolution = listLitOf IntLit $ solution strings
  compareSolutions args treeSolution expectedSolution
  where
    solution :: [String] -> [Int]
    solution = reverse . map length

-- | 12. Last Index of Zero (Q 7.7.8):
-- Given a vector of integers, at least one of which is 0, return the index of the last occurrence of 0 in the vector
testLastIndexOfZero :: TestTree
testLastIndexOfZero = QC.testProperty "Last Index Of Zero" $ do
  is <- (0 :) <$> QC.listOf1 QC.arbitrary
  let args = [listLitOf IntLit is]
      treeSolution = evalTree args lastIndexOfZeroTree
      expectedSolution = IntLit $ solution is
  compareSolutions args treeSolution expectedSolution
  where
    solution :: [Int] -> Int
    solution = fst . last . filter ((== 0) . snd) . zip [0 ..]

-- | 13. Vector Average (Q 7.7.11):
-- Given a vector of floats, return the average of those floats
testVectorAverage :: TestTree
testVectorAverage = QC.testProperty "Vector Average" $ do
  fs <- QC.listOf1 QC.arbitrary
  let args = [listLitOf FloatLit fs]
      treeSolution = evalTree args vectorAverageTree
      expectedSolution = FloatLit $ solution fs
  compareSolutions args treeSolution expectedSolution
  where
    solution :: [Float] -> Float
    solution x = sum x / fromIntegral (length x)

-- | 14. Count Odds (Q 7.7.12):
-- Given a vector of integers, return the number of integers that are odd, without use of a specific even or odd instruction.
testCountOdds :: TestTree
testCountOdds = QC.testProperty "Count Odds" $ do
  is <- QC.listOf1 QC.arbitrary
  let args = [listLitOf IntLit is]
      treeSolution = evalTree args countOddsTree
      expectedSolution = IntLit $ solution is
  compareSolutions args treeSolution expectedSolution
  where
    solution :: [Int] -> Int
    solution = length . filter odd

-- | 15. Mirror Image (Q 7.7.15):
-- Given two vectors of integers, return true if one vector is the reverse of the other, and false otherwise.
testMirrorImage :: TestTree
testMirrorImage = QC.testProperty "Mirror Image" $ do
  i1 <- QC.listOf1 QC.arbitrary
  i2 <- QC.oneof [QC.listOf1 QC.arbitrary, pure $ reverse i1]
  let args = [listLitOf IntLit i1, listLitOf IntLit i2]
      treeSolution = evalTree args mirrorImageTree
      expectedSolution = BoolLit $ solution i1 i2
  compareSolutions args treeSolution expectedSolution
  where
    solution :: [Int] -> [Int] -> Bool
    solution x y = x == reverse y

-- | 16. Super Anagrams (P 7.3):
--  Given strings x and y of lowercase letters, return true if y is a super anagram of x, which is the case if every character in x is in y.
--  To be true, y may contain extra characters, but must have at least as many copies of each character as x does.
-- TODO:

-- | 17. Sum of Squares (Q 8.5.4):
-- Given integer n, return the sum of squaring each integer in the range [1, n].
testSumOfSquares :: TestTree
testSumOfSquares = QC.testProperty "Sum of Squares" $ do
  i <- QC.getPositive <$> QC.arbitrary
  let args = [IntLit i]
      treeSolution = evalTree args sumOfSquaresTree
      expectedSolution = IntLit $ solution i
  compareSolutions args treeSolution expectedSolution
  where
    solution :: Int -> Int
    solution i = sum $ (^ 2) <$> [1 .. i]

-- | 18. Vectors Summed (Q 8.7.6):
-- Given two equal-sized vectors of integers, return a vector of integers that contains the sum of the input vectors at each index.
testVectorsSummed :: TestTree
testVectorsSummed = QC.testProperty "Vectors Summed" $ do
  i1 <- QC.listOf1 QC.arbitrary
  i2 <- QC.vector (length i1)
  let args = [listLitOf IntLit i1, listLitOf IntLit i2]
      treeSolution = evalTree args vectorsSummedTree
      expectedSolution = listLitOf IntLit $ solution i1 i2
  compareSolutions args treeSolution expectedSolution
  where
    solution :: [Int] -> [Int] -> [Int]
    solution = zipWith (+)

-- | 19. X-Word Lines (P 8.1):
-- Given an integer X and a string that can contain spaces and newlines, print the string with exactly X words per line
-- TODO: Should we add unwords?

-- | 20. Pig Latin (P 8.2):
-- Given a string containing lowercase words separated by single spaces, print the string with each word translated to pig Latin.
-- Specifically, if a word starts with a vowel, it should have “ay” added to its end; otherwise, the first letter is moved to the end of the word, followed by “ay”.
-- TODO:

-- | 21. Negative To Zero (Q 9.6.8):
-- Given a vector of integers, return the vector where all negative integers have been replaced by 0.
testNegativeToZero :: TestTree
testNegativeToZero = QC.testProperty "Negative To Zero" $ do
  i1 <- QC.listOf1 QC.arbitrary
  let args = [listLitOf IntLit i1]
      treeSolution = evalTree args negativeToZeroTree
      expectedSolution = listLitOf IntLit $ solution i1
  compareSolutions args treeSolution expectedSolution
  where
    solution :: [Int] -> [Int]
    solution = fmap (\x -> if x < 0 then 0 else x)

-- | 26. Grade
-- Given 5 integers, the first four represent the lower numeric thresholds for achieving an A, B, C, and D, and will be distinct and in descending order.
-- The fifth represents the student’s numeric grade.
-- The program must print `Student has a X grade.`, where X is A, B, C, D, or F depending on the thresholds and the numeric grade.
testGrade :: TestTree
testGrade = QC.testProperty "Grade" $ do
  ts <- reverse . sort <$> QC.vectorOf 4 (QC.chooseInt (0, 100))
  if nub ts /= ts
    then QC.discard
    else do
      studentGrade <- QC.chooseInt (0, 100)
      let [a, b, c, d] = ts
          args = IntLit <$> [a, b, c, d, studentGrade]
          treeSolution = evalTree args gradeTree
          expectedSolution = stLit $ solution a b c d studentGrade
      compareSolutions args treeSolution expectedSolution
  where
    solution :: Int -> Int -> Int -> Int -> Int -> String
    solution a b c d value = "Student has a " <> g <> " grade."
      where
        g
          | value >= a = "A"
          | value >= b = "B"
          | value >= c = "C"
          | value >= d = "D"
          | otherwise = "F"