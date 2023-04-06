module Grammar.Pretty where

import Data.Char (toLower)
import Data.List (intercalate, stripPrefix)
import qualified Data.Map as M
import Grammar.Core
import Pretty (Pretty (..))

instance Pretty Lit where
  pretty (IntLit i) = show i
  pretty (FloatLit f) = show f
  pretty (BoolLit b) = show b
  pretty (CharLit s) = show s
  pretty (ListLit GChar lits) = show $ extractChar <$> lits
    where
      extractChar (CharLit c) = c
      extractChar _ = error "Pretty: Cannot pretty list of mixed typed"
  pretty (ListLit _ lits) = "[" <> commas (pretty <$> lits) <> "]"
  pretty (LambdaLit tr) = parens $ "\\y -> " <> replace "x0" "y" (pretty tr)
  pretty (PairLit a b) = parens $ commas $ pretty <$> [a, b]

instance Pretty TypedTree where
  pretty = pretty . _tree

instance Pretty Terminal where
  pretty (Arg a) = "x" <> show a
  pretty (Literal val) = pretty val

replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace [] _ _ = error "replace, first argument cannot be empty"
replace from to xs | Just xs <- stripPrefix from xs = to ++ replace from to xs
replace from to (x : xs) = x : replace from to xs
replace from to [] = []

commas :: [String] -> String
commas = intercalate ","

args :: [Tree] -> String
args = (' ' :) . unwords . map parensIfNeeded

parensIfNeeded :: Tree -> String
parensIfNeeded t@Node {} = parens $ pretty t
parensIfNeeded t = pretty t

parens :: String -> String
parens s = "(" <> s <> ")"

lowerFirst :: String -> String
lowerFirst (x : xs) = toLower x : xs
lowerFirst x = x

binaryOperatorMap :: M.Map Operation String
binaryOperatorMap =
  M.fromList
    [ (AddInt, "+"),
      (AddFloat, "+"),
      (SubInt, "-"),
      (SubFloat, "-"),
      (MultInt, "*"),
      (MultFloat, "*"),
      (DivFloat, "/"),
      (GtInt, ">"),
      (LtInt, "<"),
      (EqInt, "=="),
      (EqChar, "=="),
      (And, "&&"),
      (Or, "||"),
      (Cons, ":")
    ]

customNameMap :: M.Map Operation String
customNameMap =
  M.fromList
    [ (MaxInt, "max"),
      (MinInt, "min"),
      (DivInt, "div"),
      (ModInt, "mod"),
      (Len, "length"),
      (SumFloats, "sum"),
      (SumInts, "sum"),
      (ProductFloats, "product"),
      (ProductInts, "product"),
      (IntToFloat, "fromIntegral")
    ]

prettyBinaryOperator :: String -> [Tree] -> String
prettyBinaryOperator op [t1, t2] = parensIfNeeded t1 <> " " <> op <> " " <> parensIfNeeded t2
prettyBinaryOperator op ts =
  error $
    "[PRTY] "
      <> show op
      <> " expects 2 arguments, but got "
      <> show (length ts)
      <> ": "
      <> pretty ts

instance Pretty Tree where
  pretty Leaf {_terminal = v} = pretty v
  pretty Node {_operation = If, _args = [a, b, c]} = unwords ["if", parensIfNeeded a, "then", parensIfNeeded b, "else", parensIfNeeded c]
  pretty Node {_operation = f, _args = ts}
    | Just op <- M.lookup f binaryOperatorMap =
      prettyBinaryOperator op ts
  pretty Node {_operation = f, _args = ts}
    | Just op <- M.lookup f customNameMap =
      op <> args ts
  pretty Node {_operation = ToPair, _args = ts} = parens . commas $ pretty <$> ts
  pretty Node {_operation = x, _args = ts} = lowerFirst (show x) <> args ts

instance {-# OVERLAPPABLE #-} Pretty a => Pretty [a] where
  pretty xs = "[" <> intercalate ", " (map pretty xs) <> "]"

instance Pretty GType where
  pretty GInt = "Int"
  pretty GFloat = "Float"
  pretty GBool = "Bool"
  pretty GChar = "Char"
  pretty (GPair a b) = parens . commas $ pretty <$> [a, b]
  pretty (GPoly n) = [toEnum (fromEnum 'a' + n)]
  pretty (GList gt) = "[" <> pretty gt <> "]"
  pretty (GLambda ft) = "(" <> pretty ft <> ")"

instance Pretty FunctionType where
  pretty ot = intercalate " -> " (pretty <$> _argTypes ot <> [_outType ot])
