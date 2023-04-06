{-
Mutation works as follows:
1. randomly select a node within the parent tree as the mutation point,
2. generate a new tree of maximum depth MAX-MUTATION-TREE-DEPTH,
3. replace the subtree rooted at the selected node with the generated tree, and
4. if the maximum depth of the child is less than or equal to MAX-TREE-DEPTH, then use it.
    If the maximum depth is greater than MAX-TREE-DEPTH, then either use the parent (Koza) or start again from scratch (STGP)

The genetic operators, like the initial tree generator, must respect the type constraints on the parse trees.
Mutation uses the same algorithm employed by the initial tree generator to create a new subtree that returns the same type
as the deleted subtree and that has internal consistency between argument types and return types.
If it is impossible to generate such a tree, then the mutation operator returns either the parent or nothing.
-}
module Evolution.Mutation (mutate) where

import Evolution.Config
  ( Config (_maxMutationTreeDepth, _maxTreeDepth, _programType),
  )
import Evolution.Core (Mutation)
import Evolution.Generate (grow)
import Evolution.Helpers (atRandomPoint)
import Grammar
import Pretty
import qualified System.Random as R

-- | Replaces that subtree with a different one, of the same output type.
replaceSubtree :: Ord a => Config a -> Mutation
replaceSubtree config tree =
  grow
    config
    (min (_maxMutationTreeDepth config) (_maxTreeDepth config - getCurrentDepth tree))
    (inferOutputType argTypes tree)
  where
    argTypes = _argTypes $ _programType config

-- | Randomly selects a node and replaces it with a new subtree that returns the same type as the deleted one.
mutate :: Ord a => Config a -> Mutation
mutate cfg = atRandomPoint (replaceSubtree cfg)
