{-# LANGUAGE OverloadedLists #-}

module Grammar.Types where

import Control.Monad (zipWithM)
import qualified Data.Vector as V
import Grammar.Core
import Grammar.Pretty

-- | Given an operation, returns its required (potentially polymorphic) input and output type
opType :: Operation -> FunctionType
-- Int
opType AddInt = [GInt, GInt] ->> GInt
opType SubInt = [GInt, GInt] ->> GInt
opType MultInt = [GInt, GInt] ->> GInt
opType DivInt = [GInt, GInt] ->> GInt
opType ModInt = [GInt, GInt] ->> GInt
opType MaxInt = [GInt, GInt] ->> GInt
opType MinInt = [GInt, GInt] ->> GInt
-- Bool
opType And = [GBool, GBool] ->> GBool
opType Or = [GBool, GBool] ->> GBool
opType Not = [GBool] ->> GBool
opType If = [GBool, GPoly 0, GPoly 0] ->> GPoly 0
-- Float
opType AddFloat = [GFloat, GFloat] ->> GFloat
opType SubFloat = [GFloat, GFloat] ->> GFloat
opType MultFloat = [GFloat, GFloat] ->> GFloat
opType DivFloat = [GFloat, GFloat] ->> GFloat
opType Sqrt = [GFloat] ->> GFloat
-- List
opType Reverse = [GList $ GPoly 0] ->> GList (GPoly 0)
opType Singleton = [GPoly 0] ->> GList (GPoly 0)
opType Cons = [GPoly 0, GList $ GPoly 0] ->> GList (GPoly 0)
opType Head = [GList $ GPoly 0] ->> GPoly 0
opType Concat = [GList $ GList $ GPoly 0] ->> GList (GPoly 0)
-- Pair
opType ToPair = [GPoly 0, GPoly 1] ->> GPair (GPoly 0) (GPoly 1)
opType Fst = [GPair (GPoly 0) (GPoly 1)] ->> GPoly 0
opType Snd = [GPair (GPoly 0) (GPoly 1)] ->> GPoly 1
-- Char + Bool
opType EqChar = [GChar, GChar] ->> GBool
opType IsLetter = [GChar] ->> GBool
opType IsDigit = [GChar] ->> GBool
-- Float + Int
opType IntToFloat = [GInt] ->> GFloat
opType Floor = [GFloat] ->> GInt
-- Int + Bool
opType GtInt = [GInt, GInt] ->> GBool
opType LtInt = [GInt, GInt] ->> GBool
opType EqInt = [GInt, GInt] ->> GBool
-- List + Int
opType Range = [GInt, GInt, GInt] ->> GList GInt
opType Len = [GList $ GPoly 0] ->> GInt
opType SumInts = [GList GInt] ->> GInt
opType ProductInts = [GList GInt] ->> GInt
opType Take = [GInt, GList $ GPoly 0] ->> GList (GPoly 0)
-- List + Float
opType SumFloats = [GList GFloat] ->> GFloat
opType ProductFloats = [GList GFloat] ->> GFloat
-- List + Char
opType Unlines = [GList (GList GChar)] ->> GList GChar
-- List + Char + Int
opType ShowInt = [GInt] ->> GList GChar
-- List + Pair
opType Zip = [GList (GPoly 0), GList (GPoly 1)] ->> GList (GPair (GPoly 0) (GPoly 1))
-- List + Lambda
opType Map = [GLambda ([GPoly 0] ->> GPoly 1), GList $ GPoly 0] ->> GList (GPoly 1)
opType Filter = [GLambda ([GPoly 0] ->> GBool), GList $ GPoly 0] ->> GList (GPoly 0)

-- | Given an operation, returns its required (potentially polymorphic) input type
opArgTypes :: Operation -> ArgTypes
opArgTypes = _argTypes . opType

-- | Given an operation, returns its required (potentially polymorphic) output type
opOutput :: Operation -> OutputType
opOutput = _outType . opType

-- | Given a literal, returns its type
litType :: Lit -> OutputType
litType (IntLit n) = GInt
litType (FloatLit x) = GFloat
litType (BoolLit b) = GBool
litType (CharLit s) = GChar
litType (PairLit a b) = GPair (litType a) (litType b)
litType (ListLit t trees) = GList t -- this assumes homogenous lists
litType (LambdaLit tt) = GLambda $ _type tt

-- | Given the arguments of a program, gets the output type of a tree in that context
getType :: ProgramArgTypes -> Tree -> OutputType
getType pgTypes Leaf {_terminal = (Arg n)} = pgTypes !! n
getType pgTypes Leaf {_terminal = (Literal lit)} = litType lit
getType pgTypes Node {_operation = op} = opOutput op

-- | Unifies two types to their most specialized version. Will fail for two distinct concrete types.
unifyTypes :: GType -> GType -> Maybe GType
unifyTypes (GPoly n) x = Just x
unifyTypes x (GPoly n) = Just x
unifyTypes (GPair a b) (GPair x y) = GPair <$> unifyTypes a x <*> unifyTypes b y
unifyTypes (GList a) (GList x) = GList <$> unifyTypes a x
unifyTypes (GLambda (MkFunctionType args1 out1)) (GLambda (MkFunctionType args2 out2)) = do
  args <- zipWithM unifyTypes args1 args2
  out <- unifyTypes out1 out2
  return $ GLambda $ MkFunctionType args out
unifyTypes a b = if a == b then Just a else Nothing

-- | Checks if a tree matches the (potentially polymorphic) expected type, given the program argument types.
-- | If it succeeds, returns the specialized concrete version of the output type
typeCheck :: ProgramType -> Tree -> Maybe OutputType
typeCheck (MkFunctionType pgArgs requiredType) t@Leaf {} = unifyTypes requiredType $ getType pgArgs t
typeCheck (MkFunctionType pgArgs requiredType) Node {_operation = op, _args = children} = do
  outType <- unifyTypes requiredType (opOutput op)
  let defaultArgTypes = opArgTypes op
      argTypes = case opOutput op of
        GPoly n -> replace (opOutput op) outType defaultArgTypes
        gt -> defaultArgTypes
  if typeCheckArgs pgArgs argTypes children
    then Just outType
    else Nothing
  where
    typeCheckArgs :: ProgramArgTypes -> [GType] -> [Tree] -> Bool
    typeCheckArgs pgArgs [] [] = True
    typeCheckArgs pgArgs [] _ = False
    typeCheckArgs pgArgs _ [] = False
    typeCheckArgs pgArgs (expectedType : ets) (t : ts) = case typeCheck (pgArgs ->> expectedType) t of
      Nothing -> False
      Just gt -> case expectedType of
        GPoly n -> typeCheckArgs pgArgs (replace (GPoly n) gt ets) ts -- if it was polymorphic, we instantiate it
        et -> typeCheckArgs pgArgs ets ts
    replace :: Eq a => a -> a -> [a] -> [a]
    replace old new = map (\el -> if el == old then new else el)

-- | Given the available program types, infers the output type of a tree.
-- This is mainly useful to determine the type for trees with many polymorphic nodes
inferOutputType :: ProgramArgTypes -> Tree -> OutputType
inferOutputType pgTypes l@Leaf {} = getType pgTypes l -- Leafs are always concrete
inferOutputType pgTypes Node {_operation = op, _args = ch} = replacePoly $ opOutput op
  where
    lookup n = snd $ head $ filter ((== GPoly n) . fst) $ learnTypes =<< zip (opArgTypes op) (inferOutputType pgTypes <$> ch)
    replacePoly (GPoly n) = lookup n
    replacePoly (GList a) = GList (replacePoly a)
    replacePoly (GPair a b) = GPair (replacePoly a) (replacePoly b)
    replacePoly (GLambda (MkFunctionType args o)) = GLambda (MkFunctionType (replacePoly <$> args) (replacePoly o))
    replacePoly t = t

learnTypes :: (GType, GType) -> [(GType, GType)]
learnTypes (GList a, GList b) = learnTypes (a, b)
learnTypes (GPair a b, GPair x y) = learnTypes (a, x) <> learnTypes (b, y)
learnTypes (GLambda (MkFunctionType as b), GLambda (MkFunctionType xs y)) = learnTypes (b, y) <> concat (zipWith (curry learnTypes) as xs)
learnTypes (x, y) = [(x, y)]