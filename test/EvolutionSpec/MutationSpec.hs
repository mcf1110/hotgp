{-# LANGUAGE TupleSections #-}

module EvolutionSpec.MutationSpec where

import Evolution
import EvolutionSpec.Helpers
import Grammar
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

tests :: [TestTree]
tests = [QC.testProperty "Mutation TypeChecks" mutationTypeChecks]

mutationTypeChecks :: QC.Gen QC.Property
mutationTypeChecks = do
  fType <- QC.arbitrary
  (config, trees) <- arbitraryRampedOfTypeWithConfig fType
  xMen <- withRandomSeed $ sequence (mutateTyped config . snd <$> trees)
  return $ QC.conjoin (validTypedTree . (_maxTreeDepth config,) <$> xMen)

mutateTyped :: Ord a => Config a -> TypedTree -> St TypedTree
mutateTyped cfg tt@(MkTypedTree t ft) = do
  newTree <- mutate cfg t
  if getHeight newTree <= _maxTreeDepth cfg
    then return $ MkTypedTree newTree ft
    else mutateTyped cfg tt
