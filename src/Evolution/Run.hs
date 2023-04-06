{-# LANGUAGE RankNTypes #-}

module Evolution.Run (runAndLog, runEvolution, Individual (_indTree, _fitness)) where

import Control.Monad (replicateM, when)
import Control.Monad.State.Strict (get, gets, put)
import qualified Control.Monad.State.Strict as S
import Data.List (find, foldl')
import Data.Maybe (fromJust)
import qualified Data.SortedList as SL
import Data.Time (UTCTime, getCurrentTime)
import Evolution.Checkpoint
import Evolution.Config
import Evolution.Core (St)
import Evolution.Crossover (crossOver)
import Evolution.Fitness (Fitness (isPerfectSolution))
import Evolution.Generate (ramped)
import Evolution.Helpers (hoistState, randomR, tupleToList)
import Evolution.Individual
import Evolution.Mutation (mutate)
import Grammar (FunctionType (_argTypes), Tree, computeMeasure, getHeight)
import Pretty (Pretty (pretty))
import System.IO (hFlush, stdout)
import System.Random.Internal (StdGen)
import Text.Printf

type LoggingFunction s a = (Maybe s -> UTCTime -> Evaluations -> SortedPop a -> IO s)

-- | Runs the evolution with the given config
runAndLog :: (Show a, Fitness a) => Config a -> LoggingFunction s a -> FilePath -> S.StateT StdGen IO (SortedPop a, Int)
runAndLog cfg loggingFunction checkpointFilename = do
  hasCheckpoint <- doesCheckpointExist checkpointFilename
  if hasCheckpoint
    then resumeFromCheckpoint
    else startFromScratch
  where
    checkpointEvery = 1000
    resumeFromCheckpoint = do
      S.liftIO $ putStrLn "LOADING CHECKPOINT"
      S.liftIO $ hFlush stdout
      (evals, pop) <- loadCheckpoint cfg checkpointFilename
      go Nothing evals pop
    startFromScratch = do
      S.liftIO $ putStrLn "STARTING FROM SCRATCH"
      S.liftIO $ hFlush stdout
      p <- hoistState $ initPop cfg
      time <- S.liftIO getCurrentTime
      -- statsCache <- S.liftIO $ loggingFunction Nothing time (length p) p
      S.liftIO $ putStrLn $ show time <> " -\t" <> show (length p) <> "/" <> show (_nEvaluations cfg)
      -- go (Just statsCache) (length p) p
      go Nothing (length p) p
    -- Main Loop:
    go maybeStatsCache nEvals pop = do
      when (nEvals `mod` checkpointEvery == 0) $ do
        S.liftIO $ putStrLn ":: CHECKPOINT ::"
        saveCheckpoint checkpointFilename nEvals pop

      (innerNEvals, newPop) <- hoistState $ steadyStateReplace cfg pop
      let totalNEvals = innerNEvals + nEvals
          pct :: Float
          pct = 100.0 * fromIntegral totalNEvals / fromIntegral (_nEvaluations cfg)
      time <- S.liftIO getCurrentTime
      -- newStatsCache <- S.liftIO $ loggingFunction maybeStatsCache time totalNEvals newPop
      S.liftIO $ printf "%s\t%d/%d (%.2f%%)\n" (show time) totalNEvals (_nEvaluations cfg) pct

      if isPerfectSolution (_fitness $ bestIndividual newPop)
        then do
          S.liftIO $ putStrLn $ show time <> " -\t" <> "Perfect solution found! " <> pretty (_indTree $ bestIndividual newPop)
          return (newPop, totalNEvals)
        else
          if totalNEvals >= _nEvaluations cfg
            then return (newPop, totalNEvals) -- DONE
            -- else go (Just newStatsCache) totalNEvals newPop
            else go Nothing totalNEvals newPop

-- | Runs the evolution with the given config
runEvolution :: (Fitness a) => Config a -> St (SortedPop a)
runEvolution cfg = do
  p <- initPop cfg
  go (length p) p
  where
    go nEvals pop = do
      (innerNEvals, newPop) <- steadyStateReplace cfg pop
      let totalNEvals = innerNEvals + nEvals
      if totalNEvals >= _nEvaluations cfg
        then return newPop
        else go totalNEvals newPop

-- | Creates the initial population with the given config
initPop :: (Fitness a) => Config a -> St (SortedPop a)
initPop cfg = SL.toSortedList . map (mkIndividual cfg) <$> ramped cfg

-- | Runs the step of the evolution, known as Steady State Replace
steadyStateReplace :: (Fitness a) => Config a -> SortedPop a -> St (Evaluations, SortedPop a)
steadyStateReplace cfg pop = do
  let rankedPop = exponentialRank cfg pop
  parents <- inPairs . map _indTree <$> sampleManyWithProb (_individualsPerStep cfg) rankedPop
  children <- concat <$> mapM (doCrossover cfg) parents
  xMen <- mapM (doMutation cfg) children

  let popTrees = map _indTree $ SL.fromSortedList pop
      withoutDuplication = filter (`notElem` popTrees) xMen
      evaluations = length children -- withoutDuplication
      newPop = keepBest cfg pop $ mkIndividual cfg <$> withoutDuplication
  return (evaluations, newPop)

-- | Given a probability, runs an action or uses a fallback
tossCoin :: Double -> a -> St a -> St a
tossCoin prob fallback action = do
  toss <- (< prob) <$> (randomR (0, 1) :: St Double)
  if not toss
    then return fallback
    else action

-- | Tosses a coin and applies the crossover
doCrossover :: Config a -> (Tree, Tree) -> St [Tree]
doCrossover cfg parents =
  tossCoin (_crossoverRate cfg) (tupleToList parents) $ maybe [] (map computeMeasure . tupleToList) <$> uncurry (crossOver cfg) parents

-- | Tosses a coin and applies the mutation
doMutation :: Ord a => Config a -> Tree -> St Tree
doMutation cfg tree = tossCoin (1 - _crossoverRate cfg) tree $ mutate cfg tree

-- | Ranks a population and gives them probabilities with an exponential falloff
exponentialRank :: Config a -> SortedPop a -> [(Individual a, Double)]
exponentialRank cfg ps = zip (SL.fromSortedList ps) $ (_parentScalar cfg **) <$> [0 ..]

-- * Survivor selection strategies

-- | Keep only the best individuals, regardless if they were parent or child
keepBest :: (Ord a) => Config a -> SortedPop a -> [Individual a] -> SortedPop a
keepBest cfg oldPop children = SL.take (_popSize cfg) $ oldPop <> SL.toSortedList children

-- | Keep all of the children, always replacing the worst parents
replaceWorst :: (Ord a) => Config a -> SortedPop a -> [Individual a] -> SortedPop a
replaceWorst cfg oldPop children = SL.toSortedList children <> SL.take (_popSize cfg - length children) oldPop

-- * Helpers

-- | Gets the list elements in pairs
inPairs :: [a] -> [(a, a)]
inPairs (x1 : (x2 : xs)) = (x1, x2) : inPairs xs
inPairs _ = []

-- | Samples values with a given probability for each value
sampleManyWithProb ::
  -- | How many values to sample
  Int ->
  -- | Each possible value paired with their probability, not necessarily normalized
  [(a, Double)] ->
  St [a]
sampleManyWithProb n xs = replicateM n sampleWithProb
  where
    (max, roulette) = cumSum xs
    sampleWithProb = do
      rand <- randomR (0, max)
      return $ fst . fromJust $ find ((> rand) . snd) roulette
    cumSum :: [(a, Double)] -> (Double, [(a, Double)])
    cumSum = fmap reverse . foldl' cumSumReducer (0, [])
      where
        cumSumReducer :: (Double, [(a, Double)]) -> (a, Double) -> (Double, [(a, Double)])
        cumSumReducer (acc, xs) (x, prob) = (newAcc, (x, newAcc) : xs)
          where
            newAcc = prob + acc
