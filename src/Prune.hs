{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Prune where

import Benchmark
import Benchmark.Dataset
import Benchmark.Log
import Data.Aeson
import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.HashMap.Strict (fromList, (!?))
import Data.Text (pack, replicate, unpack)
import Grammar (Tree, bLitT, getHeight, getNodeCount, iLitT, lambdaLitT)
import Grammar.Core
import Grammar.Helpers (computeMeasure, resetMeasure)
import Grammar.Simplify
import Grammar.Types
import Pretty (Pretty (pretty))
import System.Directory

-- | Prunes all trees within the input-prune and saves them into output-prune
prune :: IO ()
prune = do
  paths <- listDirectory "input-prune"
  mapM_ pruneFile paths

pruneFile :: FilePath -> IO ()
pruneFile fileName = do
  putStrLn fileName
  Just (Object content) <- decodeFileStrict ("input-prune/" <> fileName) :: (IO (Maybe Value))
  let Just (String showTree) = content !? "showTree"
      Just (String datasetNameText) = content !? "datasetName"
      datasetName = unpack datasetNameText
      tree = resetMeasure $ read $ unpack showTree

  (trainSet, testSet) <- case getProblem datasetName of
    Just (MkProblem problem) -> loadTrainAndTestSet "./" problem
    _ -> undefined

  let prunedTree = computeMeasure $ pruneTree trainSet tree
      (Object newJson) =
        object
          [ "showPrunedTree" .= show prunedTree,
            "stringRepPruned" .= pretty prunedTree,
            "stringRepPrunedSimple" .= pretty (simplifyTree prunedTree),
            "trainAccuracy" .= getAcc trainSet tree,
            "testAccuracy" .= getAcc testSet tree,
            "trainAccuracyPruned" .= getAcc trainSet prunedTree,
            "testAccuracyPruned" .= getAcc testSet prunedTree,
            "heightPruned" .= getHeight prunedTree,
            "nodeCountPruned" .= getNodeCount prunedTree
          ]

  encodeFile ("output-prune/" <> fileName) (Object $ content <> newJson)

getAcc :: Dataset -> Tree -> Float
getAcc dataset = fst . getAccuracyAndNmse dataset

type TreePath = [Int]

pruneTree :: Dataset -> Tree -> Tree
pruneTree dataset tree = go [] (minimumBy compareTrees [tree, simplifyTree tree])
  where
    compareTrees = compare `on` (\t -> (1 - getAcc dataset t, getHeight t, getNodeCount t))
    go path t = case replaceWithSubtrees path t of
      [] -> iterateNext t path
      subtrees ->
        let bestSubtree = minimumBy compareTrees subtrees
         in case compareTrees t bestSubtree of
              LT -> iterateNext t path -- t is better
              _ -> go path bestSubtree -- t is not better
    iterateNext t path = case nextPath t path of
      Nothing -> t -- all done!
      Just newPath -> go newPath t

nextPath :: Tree -> TreePath -> Maybe TreePath
nextPath
  Leaf {_terminal = (Literal (LambdaLit (MkTypedTree lbda _)))}
  path =
    nextPath lbda path
nextPath Leaf {} _ = Nothing
nextPath Node {} [] = Just [0]
nextPath n@Node {} (i : is)
  | Just x <- nextPath (_args n !! i) is = Just (i : x)
  | nextArg < length (_args n) = Just [nextArg]
  | otherwise = Nothing
  where
    nextArg = i + 1

replaceWithSubtrees :: TreePath -> Tree -> [Tree]
-- custom case made for map, which fails on checkPromotableArgs
-- In a->b, we can have a==b, and then the list would be promotable
replaceWithSubtrees path l@Leaf {_terminal = (Literal (LambdaLit (MkTypedTree lbda ft)))} = (`lambdaLitT` ft) <$> replaceWithSubtrees path lbda
replaceWithSubtrees [] (Node m Map [LeafLit (LambdaLit (MkTypedTree _ (MkFunctionType [a] b))), list])
  | a == b = [list]
replaceWithSubtrees [] (Node m op trs) = (trs !!) <$> checkPromotableArgs op
replaceWithSubtrees (p : ps) node@Node {} = map (setArgAt node p) $ replaceWithSubtrees ps $ _args node !! p
replaceWithSubtrees _ _ = []

-- | Checks which args are promotable, without instantiating the polymorphic function
checkPromotableArgs :: Operation -> [Int]
checkPromotableArgs (opType -> fType) = map fst $ filter ((_outType fType ==) . snd) $ zip [0 ..] $ _argTypes fType

setArgAt :: Tree -> Int -> Tree -> Tree
setArgAt node@Node {} argIndex newChild = node {_args = setAt argIndex newChild $ _args node}
setArgAt x _ _ = x

setAt :: Int -> a -> [a] -> [a]
setAt i a = go i
  where
    go 0 (_ : xs) = a : xs
    go n (x : xs) = x : go (n -1) xs
    go _ [] = []