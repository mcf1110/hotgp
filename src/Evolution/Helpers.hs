module Evolution.Helpers where

import qualified Control.Monad.State.Strict as S
import qualified Data.Vector as V
import Evolution.Core (Mutation, Point, St)
import Grammar (Tree (..), getNodeCount)
import qualified System.Random as R

-- | Selects a random value from a uniform type
random :: (R.Uniform a) => St a
random = S.state R.uniform

-- | Gets a list of random values
randoms :: (R.Random a) => St [a]
randoms = S.state (\g -> let (g1, g2) = R.split g in (R.randoms g1, g2))

-- | Selects a random value in a range from a  type
randomR :: (R.UniformRange a) => (a, a) -> St a
randomR = S.state . R.uniformR

-- | Randomly samples an element of a list (or similar)
sample :: [a] -> St a
sample [] = error "Sampling empty list!"
sample xs = do
  let l = length xs - 1
  idx <- randomR (0, l)
  return $ xs !! idx

-- | Changes a mutation to make it act on a random point
atRandomPoint :: Mutation -> Mutation
atRandomPoint f tree = do
  let n = getNodeCount tree
  point <- randomR (1, n)
  atPoint point f tree

-- | Changes a mutation to make it act on a specific point
atPoint ::
  -- | Integer representing the pre-ordered 1-indexed position (i.e., 1 [2 [3,4], 5, [6,7]]).
  Point ->
  Mutation ->
  Mutation
atPoint n f t@Leaf {} = f t
atPoint point f t@(Node measure v ch)
  | point <= 1 = f t -- found it
  | otherwise = Node measure v <$> changeChildren (point - 1) f ch -- it surely is in the children
  where
    changeChildren :: Point -> Mutation -> [Tree] -> St [Tree]
    changeChildren p f [] = return []
    changeChildren p f (t : ts) =
      if n < p
        then
          (t :)
            <$> changeChildren (p - n) f ts -- not in this child, try the next one
        else (: ts) <$> atPoint p f t -- it is in this child, lets search for it
      where
        n = getNodeCount t

tupleToList :: (a, a) -> [a]
tupleToList (x, y) = [x, y]

hoistState :: Monad m => S.State s a -> S.StateT s m a
hoistState = S.state . S.runState