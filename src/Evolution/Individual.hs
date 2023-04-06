{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Evolution.Individual where

import qualified Data.SortedList as SL
import Evolution.Config
import Evolution.Fitness
import Grammar

data Individual a where
  MkIndividual :: (Fitness a) => {_indTree :: !Tree, _fitness :: a} -> Individual a

type SortedPop a = SL.SortedList (Individual a)

deriving instance (Fitness a => Show (Individual a))

deriving instance (Fitness a => Read (Individual a))

instance Eq (Individual a) where
  i1 == i2 = _indTree i1 == _indTree i2

instance (Ord a) => Ord (Individual a) where
  compare i1 i2 = case compare (_fitness i1) (_fitness i2) of
    EQ -> compare (getNodeCount $ _indTree i1) (getNodeCount $ _indTree i2)
    x -> x

-- | Gets the best individual in a population
bestIndividual :: SortedPop a -> Individual a
bestIndividual = head . SL.fromSortedList

-- | Creates an individual
mkIndividual :: (Fitness a) => Config a -> Tree -> Individual a
mkIndividual config tree = MkIndividual computedTree (_fitnessFunction config computedTree)
  where
    computedTree = computeMeasure tree