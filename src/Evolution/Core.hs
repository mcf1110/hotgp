module Evolution.Core where

import qualified Control.Monad.State.Strict as S
import Grammar.Core (Tree)
import qualified System.Random as R

type Point = Int

type St a = S.State R.StdGen a

type Depth = Int

type PopSize = Int

type Mutation = Tree -> St Tree
