module Benchmark.Metrics where

import Data.Array (array, listArray, (!))
import Data.Semigroup
import Grammar
import Pretty

floatError :: Lit -> Lit -> Sum Float
floatError (FloatLit x) (FloatLit y) = Sum $ abs (x - y)
floatError a b = error $ "Can't apply floatError to" <> pretty a <> " and " <> pretty b

intError :: Lit -> Lit -> Sum Integer
intError (IntLit x) (IntLit y) = Sum $ abs $ toInteger x - toInteger y
intError a b = error $ "Can't apply intError to" <> pretty a <> " and " <> pretty b

boolError :: Lit -> Lit -> Sum Integer
boolError (BoolLit x) (BoolLit y) = Sum $ if x == y then 0 else 1
boolError a b = error $ "Can't apply boolError to" <> pretty a <> " and " <> pretty b

levenshteinDistance :: Lit -> Lit -> Sum Integer
levenshteinDistance a b = Sum $ toInteger $ getLevenshteinDistance (extractList a) (extractList b)

prettyLevenshteinDistance :: Lit -> Lit -> Sum Integer
prettyLevenshteinDistance a b = Sum $ toInteger $ getLevenshteinDistance (pretty a) (pretty b)

--  https://www.reddit.com/r/programming/comments/w4gs6/comment/c5a6jjz/?utm_source=share&utm_medium=web2x&context=3
getLevenshteinDistance :: (Eq a) => [a] -> [a] -> Int
getLevenshteinDistance xs ys = levMemo ! (n, m)
  where
    levMemo = array ((0, 0), (n, m)) [((i, j), lev i j) | i <- [0 .. n], j <- [0 .. m]]
    n = length xs
    m = length ys
    xa = listArray (1, n) xs
    ya = listArray (1, m) ys
    lev 0 v = v
    lev u 0 = u
    lev u v
      | xa ! u == ya ! v = levMemo ! (u -1, v -1)
      | otherwise =
        1
          + minimum
            [ levMemo ! (u, v -1),
              levMemo ! (u -1, v),
              levMemo ! (u -1, v -1)
            ]

rightWrong :: Lit -> Lit -> Sum Integer
rightWrong a b = Sum $ if a == b then 0 else 1