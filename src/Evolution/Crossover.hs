module Evolution.Crossover where

import Data.Foldable (Foldable (foldl'))
import Evolution.Config (Config (_maxTreeDepth, _programType))
import Evolution.Core (Point, St)
import Evolution.Helpers (atPoint, randomR, sample)
import Grammar
  ( FunctionType (_argTypes),
    GType,
    ProgramArgTypes,
    Tree (..),
    getCurrentDepth,
    getHeight,
    getNodeCount,
    inferOutputType,
  )

-- | Gets all subtrees from `tree` that can be crossed with `branch`
-- TODO: Write tests
getAllMatchingSubtrees :: Config a -> Tree -> Tree -> [(Point, Tree)]
getAllMatchingSubtrees cfg branch tree = snd $ go 1 tree
  where
    argTypes = _argTypes $ _programType cfg
    desiredOutType = inferOutputType argTypes branch
    satisfies candidateBranch =
      inferOutputType argTypes candidateBranch == desiredOutType -- types match
        && candidateBranch `fitsIn` branch
        && branch `fitsIn` candidateBranch

    fitsIn :: Tree -> Tree -> Bool
    fitsIn src dest = getCurrentDepth dest + getHeight src <= _maxTreeDepth cfg

    go n t@Node {_args = trs} = (nChildren, this ++ children)
      where
        this = [(n, t) | satisfies t]
        (nChildren, children) = foldl' f (n + 1, []) trs
        f (p, ls) tr = (ls ++) <$> go p tr
    go n t@Leaf {} = (n + 1, this)
      where
        this = [(n, t) | satisfies t]

-- TODO: Write tests
getBranchAt :: Point -> Tree -> Tree
getBranchAt p t@Node {_args = ch}
  | p > 1 =
    getChildren (p - 1) ch
  where
    getChildren p (t : ts) =
      let n = getNodeCount t
       in if n < p
            then getChildren (p - n) ts
            else getBranchAt p t
    getChildren _ _ = error "This should never happen"
getBranchAt _ t = t

-- | Performs the crossover of two trees
crossOver :: Config a -> Tree -> Tree -> St (Maybe (Tree, Tree))
crossOver cfg t1 t2 = do
  p1 <- randomR (1, getNodeCount t1)
  let b1 = getBranchAt p1 t1
      matchingT2 = getAllMatchingSubtrees cfg b1 t2

  case matchingT2 of
    [] -> return Nothing
    m -> do
      (p2, b2) <- sample m
      c1 <- atPoint p1 (const $ return b2) t1
      c2 <- atPoint p2 (const $ return b1) t2
      return $ Just (c1, c2)
