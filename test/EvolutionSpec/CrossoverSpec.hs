module EvolutionSpec.CrossoverSpec where

import Evolution (Config (_maxTreeDepth))
import Evolution.Crossover
import EvolutionSpec.Helpers
import Grammar
import Pretty
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

tests :: [TestTree]
tests =
  [ QC.testProperty "Crossover TypeChecks" crossoverTypeChecks,
    QC.testProperty "Crossover Never Exceed Depth" crossoverNeverExceedsDepth
  ]

crossoverTypeChecks :: QC.Gen QC.Property
crossoverTypeChecks = do
  argCount <- QC.choose (0, 5)
  fType <- QC.resize argCount QC.arbitrary
  (cfg, trees) <- QC.resize 2 $ arbitraryRampedOfTypeWithConfig fType
  let [(_, t1), (_, t2)] = trees
      tCheck t = runTypeCheck $ MkTypedTree t fType
  maybePair <- withRandomSeed $ crossOver cfg (_tree t1) (_tree t2)
  let p = maybe QC.discard (\(ta, tb) -> tCheck ta QC..&&. tCheck tb) maybePair
  return $
    QC.counterexample
      ( "Failed for crossover: \n\t"
          <> pretty (_tree t1)
          <> "\nWITH\n\t"
          <> pretty (_tree t2)
          <> "\n--------\n"
      )
      p

crossoverNeverExceedsDepth :: QC.Gen QC.Property
crossoverNeverExceedsDepth = do
  argCount <- QC.choose (0, 5)
  fType <- QC.resize argCount QC.arbitrary
  (cfg, trees) <- QC.resize 2 $ arbitraryRampedOfTypeWithConfig fType
  let [(_, t1), (_, t2)] = trees
      isValid t = getHeight (computeMeasure t) <= _maxTreeDepth cfg
  maybePair <-
    withRandomSeed $
      crossOver
        cfg
        (computeMeasure $ _tree t1)
        (computeMeasure $ _tree t2)
  case maybePair of
    Nothing -> QC.discard
    Just (ta, tb) -> do
      let p = maybe QC.discard (\(ta, tb) -> isValid ta QC..&&. isValid tb) maybePair
      return $
        QC.counterexample
          ( unlines
              [ "Failed for crossover",
                "Max Depth " <> show (_maxTreeDepth cfg),
                pretty ta,
                "Height: " <> show (getHeight ta),
                "WITH",
                pretty tb,
                "Height: " <> show (getHeight tb),
                "--------",
                ""
              ]
          )
          p
