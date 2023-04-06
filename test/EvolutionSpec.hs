module EvolutionSpec where

import qualified EvolutionSpec.AtPointSpec
import qualified EvolutionSpec.CrossoverSpec
import qualified EvolutionSpec.GenerateSpec
import qualified EvolutionSpec.MutationSpec
import Test.Tasty

tests :: [TestTree]
tests =
  [ testGroup
      "Crossover"
      EvolutionSpec.CrossoverSpec.tests,
    testGroup
      "Generate - Type Check"
      EvolutionSpec.GenerateSpec.tests,
    testGroup
      "Mutation"
      EvolutionSpec.MutationSpec.tests,
    testGroup "At Point" EvolutionSpec.AtPointSpec.tests
  ]
