module Grammar.Helpers where

import Data.Maybe (fromJust)
import Grammar.Core
import Grammar.Types (litType)

mkLeaf :: Terminal -> Tree
mkLeaf = Leaf Nothing

litT :: Lit -> Tree
litT = LeafLit

arg :: Int -> Tree
arg = LeafArg

stLit :: String -> Lit
stLit = tListLitOf GChar CharLit

pairLit :: (Lit, Lit) -> Lit
pairLit (a, b) = PairLit a b

lambdaLit :: Tree -> FunctionType -> Lit
lambdaLit a b = LambdaLit $ MkTypedTree a b

listLitOf :: (a -> Lit) -> [a] -> Lit
listLitOf f xs = tListLitOf t f xs
  where
    t = litType $ f (head xs)

tListLitOf :: GType -> (a -> Lit) -> [a] -> Lit
tListLitOf t f xs = ListLit t $ map f xs

iLitT :: Int -> Tree
iLitT = litT . IntLit

fLitT :: Float -> Tree
fLitT = litT . FloatLit

bLitT :: Bool -> Tree
bLitT = litT . BoolLit

chLitT :: Char -> Tree
chLitT = litT . CharLit

stLitT :: String -> Tree
stLitT = litT . stLit

pairLitT :: (Lit, Lit) -> Tree
pairLitT = litT . pairLit

lambdaLitT :: Tree -> FunctionType -> Tree
lambdaLitT a b = litT $ lambdaLit a b

listConsNode :: GType -> [Tree] -> Tree
listConsNode gType = foldr (\acc t -> Cons <| [acc, t]) (litT $ ListLit gType [])

getMeasure :: Tree -> Measure
getMeasure t = case _measure t of
  Nothing -> getMeasure $ computeMeasure t
  Just m -> m

getHeight :: Tree -> Int
getHeight = _height . getMeasure

getNodeCount :: Tree -> Int
getNodeCount = _nodeCount . getMeasure

getCurrentDepth :: Tree -> Int
getCurrentDepth = _currentDepth . getMeasure

computeMeasure :: Tree -> Tree
computeMeasure = go 0
  where
    go currentDepth l@Leaf {} =
      l
        { _measure =
            Just
              ( MkMeasure
                  { _currentDepth = currentDepth,
                    _height = 0,
                    _nodeCount = 1
                  }
              )
        }
    go currentDepth n@Node {_args = trees} = n {_measure = Just measure, _args = newTrees}
      where
        newTrees = go (currentDepth + 1) <$> trees
        measure =
          MkMeasure
            { _currentDepth = currentDepth,
              _height = 1 + maximum (getHeight <$> newTrees),
              _nodeCount = 1 + sum (getNodeCount <$> newTrees)
            }

resetMeasure :: Tree -> Tree
resetMeasure = go 0
  where
    go currentDepth l@Leaf {} =
      l
        { _measure = Nothing
        }
    go currentDepth n@Node {_args = trees} = n {_measure = Nothing, _args = newTrees}
      where
        newTrees = go (currentDepth + 1) <$> trees