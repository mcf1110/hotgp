module Grammar.Eval (evalTree, extractChar, extractFloat, extractInt, extractString, extractList) where

import Control.Monad
import Data.Char (isAlpha, isDigit)
import Data.List (foldl', intercalate)
import Data.Maybe (mapMaybe)
import Grammar.Core
import Grammar.Helpers (pairLit, stLit, tListLitOf)
import Grammar.Types
import Pretty (pretty)

-- | Evaluates a Tree with the given arguments
evalTree :: [Lit] -> Tree -> Maybe Lit
evalTree args (LeafArg i) =
  if i < length args
    then Just $ args !! i
    else error $ "[EVAL] Could not get $" <> show i <> " from only " <> show (length args) <> " arguments: " <> pretty args
evalTree args (LeafLit v) = Just v
evalTree args Node {_operation = f, _args = opArgs} = do
  evaluatedArgs <- sequence $ evalTree args <$> opArgs
  v <- eval f evaluatedArgs
  case v of
    FloatLit x -> do
      guard (not $ isNaN x)
      return v
    _ -> return v

eval :: Operation -> [Lit] -> Maybe Lit
-- Int
eval AddInt [IntLit i1, IntLit i2] = Just $ IntLit (i1 + i2)
eval SubInt [IntLit i1, IntLit i2] = Just $ IntLit (i1 - i2)
eval MultInt [IntLit i1, IntLit i2] = Just $ IntLit (i1 * i2)
eval DivInt [IntLit _, IntLit 0] = Nothing
eval DivInt [IntLit i1, IntLit (-1)] = Just $ IntLit $ negate i1
eval DivInt [IntLit i1, IntLit i2] = Just $ IntLit $ i1 `div` i2
eval ModInt [IntLit _, IntLit 0] = Nothing
eval ModInt [IntLit i1, IntLit i2] = Just $ IntLit $ i1 `mod` i2
eval MaxInt [IntLit i1, IntLit i2] = Just $ IntLit (max i1 i2)
eval MinInt [IntLit i1, IntLit i2] = Just $ IntLit (min i1 i2)
-- Bool
eval And [BoolLit b1, BoolLit b2] = Just $ BoolLit (b1 && b2)
eval Or [BoolLit b1, BoolLit b2] = Just $ BoolLit (b1 || b2)
eval Not [BoolLit b1] = Just $ BoolLit (not b1)
eval If [BoolLit cond, a, b] = Just $ if cond then a else b
-- Float
eval AddFloat [FloatLit f1, FloatLit f2] = Just $ FloatLit (f1 + f2)
eval SubFloat [FloatLit f1, FloatLit f2] = Just $ FloatLit (f1 - f2)
eval MultFloat [FloatLit f1, FloatLit f2] = Just $ FloatLit (f1 * f2)
eval DivFloat [FloatLit f1, FloatLit f2] = if f2 == 0 then Nothing else Just $ FloatLit $ f1 / f2
eval Sqrt [FloatLit f] = Just $ FloatLit (sqrt $ abs f)
-- Char + Bool
eval EqChar [CharLit c1, CharLit c2] = Just $ BoolLit (c1 == c2)
eval IsLetter [CharLit c] = Just $ BoolLit (isAlpha c)
eval IsDigit [CharLit c] = Just $ BoolLit (isDigit c)
-- Float + Int
eval IntToFloat [IntLit i] = Just $ FloatLit (fromIntegral i)
eval Floor [FloatLit f] = Just $ IntLit (floor f)
-- Int + Bool
eval GtInt [IntLit i1, IntLit i2] = Just $ BoolLit (i1 > i2)
eval LtInt [IntLit i1, IntLit i2] = Just $ BoolLit (i1 < i2)
eval EqInt [IntLit i1, IntLit i2] = Just $ BoolLit (i1 == i2)
-- Lists
eval Len [ListLit _ l] = Just $ IntLit (length l)
eval Range [IntLit start, IntLit stop, IntLit step]
  | start == stop = Just $ ListLit GInt [IntLit start]
  | step == 0 || signum step /= signum (stop - start) = Just $ ListLit GInt []
  | otherwise = Just $ ListLit GInt $ take 5000 $ IntLit <$> [start, (start + step) .. stop]
eval Cons [a, ListLit t l] = Just $ ListLit t (a : l)
eval Singleton [a] = Just $ ListLit (litType a) [a]
eval Head [ListLit gt l] = if null l then Nothing else Just $ head l
eval Reverse [ListLit t l] = Just $ ListLit t (reverse l)
eval Map [LambdaLit f, ListLit _ l] = ListLit (_outType $ _type f) <$> mapM (evalLambda f) l
eval Concat [ListLit (GList t) l] = Just $ ListLit t $ foldl' append [] l
  where
    append :: [Lit] -> Lit -> [Lit]
    append acc (ListLit _ ls) = acc <> ls
    append _ v = error $ "[EVAL] Concat got ill-typed expression: " <> show v
eval Filter [LambdaLit f, ListLit listType l] = ListLit listType <$> applyFilter l
  where
    applyFilter :: [Lit] -> Maybe [Lit]
    applyFilter [] = Just []
    applyFilter (t : ts) = do
      result <- evalLambda f t
      case result of
        BoolLit True -> (t :) <$> applyFilter ts
        BoolLit False -> applyFilter ts
        x -> error $ "[EVAL] Filter's predicate returned " <> show x <> ", which is not a Bool!"
eval Zip [ListLit t1 l1, ListLit t2 l2] = Just $ ListLit (GPair t1 t2) $ zipWith (curry pairLit) l1 l2
eval Take [IntLit n, ListLit t l] = Just $ ListLit t (take n l)
eval SumInts [ListLit GInt l] = Just $ IntLit $ sum $ extractInt <$> l
eval ProductInts [ListLit GInt l] = Just $ IntLit $ product $ extractInt <$> l
eval SumFloats [ListLit GFloat l] = Just $ FloatLit $ sum $ extractFloat <$> l
eval ProductFloats [ListLit GFloat l] = Just $ FloatLit $ product $ extractFloat <$> l
eval Unlines [ListLit (GList GChar) strings] = Just $ tListLitOf GChar CharLit $ intercalate "\n" $ extractString <$> strings -- the actual unlines puts a \n at the end
eval ShowInt [IntLit x] = Just $ stLit $ show x
-- Pair
eval ToPair [a, b] = Just $ PairLit a b
eval Fst [PairLit a b] = Just a
eval Snd [PairLit a b] = Just b
eval f vs = error $ "Cannot apply " <> show f <> " to " <> show vs

evalLambda :: TypedTree -> Lit -> Maybe Lit
evalLambda f l = evalTree [l] (_tree f)

extractFloat :: Lit -> Float
extractFloat (FloatLit f) = f
extractFloat l = error $ "[EVAL] Cannot extract Float from " <> pretty l

extractInt :: Lit -> Int
extractInt (IntLit f) = f
extractInt l = error $ "[EVAL] Cannot extract Int from " <> pretty l

extractChar :: Lit -> Char
extractChar (CharLit f) = f
extractChar l = error $ "[EVAL] Cannot extract Char from " <> pretty l

extractString :: Lit -> String
extractString (ListLit GChar ls) = extractChar <$> ls
extractString l = error $ "[EVAL] Cannot extract String from " <> pretty l

extractList :: Lit -> [Lit]
extractList (ListLit _ ls) = ls
extractList l = error $ "[EVAL] Cannot extract List from " <> pretty l
