module Grammar.Simplify where

import Data.Function (on)
import Data.List (minimumBy, permutations)
import Grammar

simplifyTree :: Tree -> Tree
simplifyTree Node {_operation = op, _args = trs}
  | all isLit simplifiedChildren = maybe newTree LeafLit (evalTree [] newTree)
  | otherwise = newTree
  where
    simplifiedChildren = simplifyTree <$> trs
    newTree
      | op `elem` commutativeOps = minimumBy (compare `on` getNodeCount) $ applyRules . (op <|) <$> permutations simplifiedChildren
      | otherwise = applyRules $ op <| simplifiedChildren
      where
        commutativeOps = [AddInt, MultInt, AddFloat, MultFloat, And, Or, EqInt, EqChar]
simplifyTree (LeafLit (LambdaLit (MkTypedTree tree ft))) = lambdaLitT (simplifyTree tree) ft
simplifyTree x = x

applyAssociativeRules :: Tree -> Maybe Tree
applyAssociativeRules
  Node
    { _operation = op1,
      _args = [LeafLit a, Node {_operation = op2, _args = [LeafLit b, x]}]
    }
    | op1 == op2 = Just $ op1 <| [op1 <| [LeafLit a, LeafLit b], x]
applyAssociativeRules
  Node
    { _operation = op1,
      _args = [LeafLit a, Node _ op2 [x, LeafLit b]]
    }
    | op1 == op2 = Just $ op1 <| [op1 <| [LeafLit a, LeafLit b], x]
applyAssociativeRules _ = Nothing

applyRules :: Tree -> Tree
applyRules Node {_operation = op, _args = trs} = maybe (op <| trs) simplifyTree (go op trs)
  where
    go :: Operation -> [Tree] -> Maybe Tree
    go op trs
      | op `elem` associativeOps,
        Just simplified <- applyAssociativeRules $ op <| trs =
        Just simplified
      where
        associativeOps = [AddInt, AddFloat, MultInt, MultFloat, And, Or, MaxInt, MinInt]
    go If [LeafLit (BoolLit True), used, _] = Just used
    go If [LeafLit (BoolLit False), _, used] = Just used
    go If [_, a, b] | a == b = Just a
    go EqInt [a, b] | a == b = Just $ bLitT True
    go LtInt [a, b] | a == b = Just $ bLitT False
    go GtInt [a, b] | a == b = Just $ bLitT False
    go MaxInt [a, b] | a == b = Just a
    go MinInt [a, b] | a == b = Just a
    go And [a, b] | a == b = Just a
    go Or [a, b] | a == b = Just a
    go EqChar [a, b] | a == b = Just $ bLitT True
    go SubInt [a, b] | a == b = Just $ iLitT 0
    go SubFloat [a, b] | a == b = Just $ fLitT 0
    go DivInt [a, b] | a == b = Just $ iLitT 1
    go ModInt [a, b] | a == b = Just $ iLitT 0
    go DivFloat [a, b] | a == b = Just $ fLitT 1
    go DivInt [Node {_operation = MultInt, _args = [a, b]}, c]
      | a == c = Just b
      | b == c = Just a
    go ModInt [Node {_operation = MultInt, _args = [a, b]}, c] | a == c || b == c = Just $ iLitT 0
    go DivFloat [Node {_operation = MultFloat, _args = [a, b]}, c]
      | a == c = Just b
      | b == c = Just a
    --
    go Fst [Node {_operation = ToPair, _args = [a, b]}] = Just a
    go Snd [Node {_operation = ToPair, _args = [a, b]}] = Just b
    --
    go Or [LeafLit (BoolLit True), _] = Just $ bLitT True
    go Or [LeafLit (BoolLit False), x] = Just x
    go And [LeafLit (BoolLit False), _] = Just $ bLitT False
    go And [LeafLit (BoolLit True), x] = Just x
    --
    go AddInt [LeafLit (IntLit 0), x] = Just x
    go AddFloat [LeafLit (FloatLit 0), x] = Just x
    go AddFloat [x, LeafLit (FloatLit 0)] = Just x
    go SubInt [x, LeafLit (IntLit 0)] = Just x
    go SubFloat [x, LeafLit (FloatLit 0)] = Just x
    go MultInt [LeafLit (IntLit 1), x] = Just x
    go MultFloat [LeafLit (FloatLit 1), x] = Just x
    go MultInt [LeafLit (IntLit 0), x] = Just $ iLitT 0
    go MultFloat [LeafLit (FloatLit 0), x] = Just $ fLitT 0
    go DivInt [x, LeafLit (IntLit 1)] = Just x
    go ModInt [x, LeafLit (IntLit 1)] = Just $ iLitT 0
    go DivFloat [x, LeafLit (FloatLit 1)] = Just x
    --
    go Len [LeafLit (ListLit _ ls)] = Just $ iLitT (length ls)
    go Len [Node {_operation = Singleton}] = Just $ iLitT 1
    go Len [Node {_operation = Cons, _args = [x, xs]}] = Just $ AddInt <| [iLitT 1, Len <| [xs]]
    go Len [Node {_operation = Reverse, _args = x}] = Just $ Len <| x
    go Head [Node {_operation = Singleton, _args = [x]}] = Just x
    go Reverse [Node {_operation = Singleton, _args = x}] = Just $ Singleton <| x
    go SumInts [Node {_operation = Singleton, _args = [x]}] = Just x
    go ProductInts [Node {_operation = Singleton, _args = [x]}] = Just x
    go Reverse [Node {_operation = Reverse, _args = [x]}] = Just x
    go Take [Node {_operation = Len, _args = [x]}, y] | x == y = Just x
    go Range [start, stop, _] | start == stop = Just $ Singleton <| [start]
    --
    go If [Node {_operation = Not, _args = [cond]}, a, b] = Just $ If <| [cond, b, a]
    go EqInt [Node {_operation = If, _args = [cond, LeafLit a, LeafLit b]}, LeafLit ref] = Just $ applyEqualIfRule cond a b ref
    go EqInt [LeafLit ref, Node {_operation = If, _args = [cond, LeafLit a, LeafLit b]}] = Just $ applyEqualIfRule cond a b ref
    go op trs = Nothing
applyRules x = x

applyEqualIfRule :: Tree -> Lit -> Lit -> Lit -> Tree
applyEqualIfRule cond a b reference
  | a == reference = cond
  | b == reference = Not <| [cond]
  | otherwise = bLitT False

isLit :: Tree -> Bool
isLit Leaf {_terminal = (Literal lit)} = True
isLit _ = False
