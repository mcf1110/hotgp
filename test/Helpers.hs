module Helpers where

import Data.List (find)
import Grammar
import qualified Test.Tasty.QuickCheck as QC

genLitOfType :: GType -> QC.Gen Lit
genLitOfType GInt = IntLit . QC.getPositive <$> QC.arbitrary
genLitOfType GFloat = FloatLit . QC.getPositive <$> QC.arbitrary
genLitOfType GBool = BoolLit <$> QC.arbitrary
genLitOfType GChar = CharLit <$> QC.arbitraryASCIIChar
genLitOfType (GPair a b) = PairLit <$> genLitOfType a <*> genLitOfType b
genLitOfType (GList t) = ListLit t <$> QC.listOf1 (genLitOfType t)
genLitOfType (GLambda ft) = flip lambdaLit ft . LeafLit <$> genLitOfType (_outType ft) -- const functions
genLitOfType (GPoly n) = error "Cannot generate lit of Polymorphic type"

type ConstraintMap = [(Int, GType)]

instantiatePolymorphicType :: FunctionType -> QC.Gen FunctionType
instantiatePolymorphicType = instantiatePolymorphicTypeWith []

instantiatePolymorphicTypeWith :: ConstraintMap -> FunctionType -> QC.Gen FunctionType
instantiatePolymorphicTypeWith cm ft = do
  rands <- QC.infiniteListOf arbitraryType
  return $ replaceFunctionType cm rands ft

arbitraryType :: QC.Gen GType
arbitraryType =
  QC.elements $
    concreteTypes
      <> (GList <$> concreteTypes)
      <> (GPair <$> concreteTypes <*> concreteTypes)
  where
    concreteTypes = [GInt, GBool, GFloat, GChar]

replaceFunctionType :: ConstraintMap -> [GType] -> FunctionType -> FunctionType
replaceFunctionType cm rands (MkFunctionType i o) = (rep <$> i) ->> rep o
  where
    rep :: GType -> GType
    rep (GPoly n) = case find ((== n) . fst) cm of
      Just (n, gt) -> gt
      Nothing -> rands !! n
    rep (GList a) = GList $ rep a
    rep (GPair a b) = GPair (rep a) (rep b)
    rep (GLambda ft) = GLambda (replaceFunctionType cm rands ft)
    rep t = t
