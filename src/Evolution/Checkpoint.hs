{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Evolution.Checkpoint (doesCheckpointExist, loadCheckpoint, saveCheckpoint) where

import qualified Control.Monad.State.Strict as S
import qualified Data.ByteString as B
import Data.Either (fromRight)
import qualified Data.SortedList as SL
import Data.Word (Word64)
import Evolution.Config (Config)
import Evolution.Core (St)
import Evolution.Fitness
import Evolution.Helpers (hoistState)
import Evolution.Individual
import Flat
import GHC.Generics (Generic)
import Grammar
import System.Directory (doesFileExist, removeFile)
import System.Random.Internal (StdGen (..), unStdGen)
import System.Random.SplitMix (seedSMGen', unseedSMGen)

data Checkpoint = MkCheckpoint
  { _gen :: (Word64, Word64),
    _population :: [Tree],
    _currentEvals :: Int
  }
  deriving (Show, Read, Generic, Flat)

deriving instance Flat TypedTree

deriving instance Flat Operation

deriving instance Flat FunctionType

deriving instance Flat GType

deriving instance Flat Lit

deriving instance Flat Terminal

deriving instance Flat Measure

deriving instance Flat Tree

type CheckpointFilename = FilePath

makeCheckpoint :: Int -> SortedPop a -> St Checkpoint
makeCheckpoint nEvals pop = do
  gen <- S.gets unStdGen
  return $
    MkCheckpoint
      { _gen = unseedSMGen gen,
        _population = _indTree <$> SL.fromSortedList pop,
        _currentEvals = nEvals
      }

decodeCheckpoint :: (Fitness a) => Config a -> Checkpoint -> St (Int, SortedPop a)
decodeCheckpoint cfg checkpoint = do
  S.put (StdGen $ seedSMGen' $ _gen checkpoint)
  return (_currentEvals checkpoint, SL.toSortedList $ mkIndividual cfg <$> _population checkpoint)

doesCheckpointExist :: CheckpointFilename -> S.StateT StdGen IO Bool
doesCheckpointExist = S.liftIO . doesFileExist

loadCheckpoint :: (Fitness a) => Config a -> CheckpointFilename -> S.StateT StdGen IO (Int, SortedPop a)
loadCheckpoint cfg filename = do
  contents <- S.liftIO $ B.readFile filename
  hoistState $ decodeCheckpoint cfg $ fromRight (error $ "Invalid checkpoint in " <> filename) $ unflat contents

saveCheckpoint :: (Fitness a) => CheckpointFilename -> Int -> SortedPop a -> S.StateT StdGen IO ()
saveCheckpoint filename nEvals pop = do
  checkpoint <- hoistState $ makeCheckpoint nEvals pop
  S.liftIO $ B.writeFile filename $ flat checkpoint
