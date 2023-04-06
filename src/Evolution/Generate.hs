module Evolution.Generate where

import Control.Monad (join, replicateM)
import qualified Data.Map as M
import Evolution.Config
import Evolution.Core (Depth, PopSize, St)
import Evolution.Helpers (sample)
import Evolution.SamplerTable (buildTree)
import Grammar (ArgTypes, FunctionType (_argTypes, _outType), Operation, OutputType, Tree (..))
import Grammar.Core

-- | Generates a tree that is of full depth along any path.
full ::
  -- | The configuration of the desired program
  Config a ->
  -- | The depth that this tree will have
  Depth ->
  -- | The type that this tree will output
  OutputType ->
  St Tree
full cfg = buildTree (_fullTable cfg)

-- | Generates a tree that might be smaller than the desired output.
grow ::
  -- | The configuration of the desired program
  Config a ->
  -- | The maximum depth that this tree will have
  Depth ->
  -- | The type that this tree will output
  OutputType ->
  St Tree
grow cfg = buildTree (_growTable cfg)

-- | "Ramped-half-and-half" generates trees of all different shapes and sizes.
-- This is Koza's standard approach to generating an initial population.
-- It uses the full method to generate half the members and the grow method to generate the other half.
ramped ::
  -- | The configuration of the desired program
  Config a ->
  St [Tree]
ramped cfg = go [] (_popSize cfg) 2
  where
    go :: [Tree] -> PopSize -> Depth -> St [Tree]
    go pop 0 _ = return pop
    go pop nPop minDepth = do
      treesFull <- replicateM half (full cfg minDepth outputType)
      treesGrow <- replicateM (half + r) (grow cfg minDepth outputType)
      go (treesFull <> treesGrow <> pop) (nPop - n) (minDepth + 1)
      where
        range = maxDepth - minDepth + 1
        n = nPop `div` range
        (half, r) = quotRem n 2

    outputType :: OutputType
    outputType = _outType $ _programType cfg
    maxDepth :: Depth
    maxDepth = _maxInitialDepth cfg
