{-# LANGUAGE TypeApplications #-}

module GrammarTypesSpec where

import Grammar
import Helpers (genLitOfType, instantiatePolymorphicType)
import Test.Tasty
import Test.Tasty.QuickCheck (Arbitrary (arbitrary))
import qualified Test.Tasty.QuickCheck as QC
import qualified Text.ParserCombinators.ReadP as QC

tests :: [TestTree]
tests =
  [ QC.testProperty
      "All operations run with the provided types"
      testOperations
  ]

testOperations :: QC.Gen QC.Property
testOperations = do
  operation <- QC.arbitraryBoundedEnum @Operation
  operationType <- instantiatePolymorphicType $ opType operation
  lits <- sequence $ genLitOfType <$> _argTypes operationType
  let evaluated = evalTree [] (operation <| (litT <$> lits))
  case evaluated of
    Nothing -> QC.discard
    Just e -> return $ QC.counterexample (show (operation <| (litT <$> lits))) $ litType e QC.=== _outType operationType
