module Evolution.Fitness where

import Benchmark.Helpers (Result (Error, Result))
import Data.Monoid (Sum (Sum, getSum))

-- | Values that can be used as fitness
class (Ord a, Show a, Read a) => Fitness a where
  -- | How to log this fitness in the csv
  logFitness :: a -> String

  -- | Does this fitness represent a perfect solution?
  isPerfectSolution :: a -> Bool

instance Fitness a => Fitness (Result a) where
  logFitness (Result x) = logFitness x
  logFitness Error = "nan"
  isPerfectSolution (Result x) = isPerfectSolution x
  isPerfectSolution Error = False

instance Fitness a => Fitness (Sum a) where
  logFitness = logFitness . getSum
  isPerfectSolution = isPerfectSolution . getSum

instance Fitness Float where
  logFitness = show
  isPerfectSolution x = x < 1e-3

instance Fitness Int where
  logFitness = show
  isPerfectSolution x = x <= 0

instance Fitness Integer where
  logFitness = show
  isPerfectSolution x = x <= 0