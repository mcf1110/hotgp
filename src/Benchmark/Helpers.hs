module Benchmark.Helpers where

import Control.Monad.State.Strict (MonadState (state))
import Data.List (intercalate)
import qualified Data.Map as M
import Evolution.Core (St)
import Grammar
import System.Random

data Result a = Result !a | Error deriving (Eq, Ord, Show, Read) -- custom Maybe datatype for custom Ord, where Error is greater than everything

maybeToResult :: Maybe a -> Result a
maybeToResult Nothing = Error
maybeToResult (Just a) = Result a

instance Semigroup a => Semigroup (Result a) where
  (Result x) <> (Result y) = Result $ x <> y
  _ <> _ = Error

instance Monoid a => Monoid (Result a) where
  mempty = Result mempty

instance Functor Result where
  fmap f (Result x) = Result (f x)
  fmap f Error = Error

fisherYatesStep :: RandomGen g => (M.Map Int a, g) -> (Int, a) -> (M.Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((M.insert j x . M.insert i (m M.! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

allowList :: [GType] -> [GType]
allowList gTypes = gTypes <> (GList <$> gTypes)

allowPairs :: [GType] -> [GType]
allowPairs gTypes = gTypes <> (GPair <$> gTypes <*> gTypes)

allowUnaryLambdas :: [GType] -> [GType]
allowUnaryLambdas gTypes = gTypes <> (GLambda <$> (MkFunctionType <$> ((: []) <$> gTypes) <*> gTypes))

csvJoin :: [String] -> String
csvJoin = intercalate ";"