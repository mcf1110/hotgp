module EvolutionSpec.GenerateSpec where

import Control.Monad.State.Strict (evalState)
import Evolution
import EvolutionSpec.Helpers
import Grammar
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

tests :: [TestTree]
tests =
  [ QC.testProperty "full" $ QC.forAll (arbitraryTree full) validTypedTree,
    QC.testProperty "grow" $ QC.forAll (arbitraryTree grow) validTypedTree,
    QC.testProperty "ramped" $
      QC.forAll arbitraryRamped $
        QC.conjoin . fmap validTypedTree,
    QC.testProperty "ramped are all same type" $
      QC.forAll arbitraryRamped $ \trees -> let types = _type . snd <$> trees in all (== head types) types
  ]
