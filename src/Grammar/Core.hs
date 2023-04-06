{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

module Grammar.Core where

import GHC.Generics (Generic)

-- * Data Types

-- | Types supported by the grammar
data GType
  = GInt
  | GFloat
  | GBool
  | GChar
  | GPair !GType !GType
  | -- | GPoly 0 would be "t0"
    GPoly !Int
  | -- | [a]
    GList !GType
  | -- | a -> b
    GLambda FunctionType
  deriving (Show, Eq, Ord, Read, Generic)

-- | List of types of the arguments expected by the function
type ArgTypes = [GType]

-- | Type outputted by the function
type OutputType = GType

-- Defines the arguments and output types for a function
data FunctionType = MkFunctionType
  { _argTypes :: !ArgTypes,
    _outType :: !OutputType
  }
  deriving (Show, Eq, Ord, Read, Generic)

-- Defines the arguments and output types for the whole program
type ProgramType = FunctionType

-- | List of types of the arguments expected by the program
type ProgramArgTypes = ArgTypes

-- | A concrete representation of a value
data Lit
  = IntLit !Int
  | FloatLit !Float
  | BoolLit !Bool
  | CharLit !Char
  | PairLit !Lit !Lit
  | ListLit !GType ![Lit]
  | LambdaLit !TypedTree
  deriving (Show, Eq, Read, Generic)

data Measure = MkMeasure {_currentDepth :: !Int, _height :: !Int, _nodeCount :: !Int} deriving (Show, Eq, Read, Generic)

-- | Values that do not receive inputs
data Terminal
  = Arg !Int
  | Literal !Lit
  deriving (Show, Eq, Read, Generic)

-- | The structure that represents a program written in this grammar
data Tree
  = Leaf {_measure :: !(Maybe Measure), _terminal :: !Terminal}
  | Node {_measure :: !(Maybe Measure), _operation :: !Operation, _args :: ![Tree]}
  deriving (Show, Eq, Read, Generic)

-- | A program combined with its expected inputs and output types
data TypedTree = MkTypedTree
  { _tree :: !Tree,
    _type :: !FunctionType
  }
  deriving (Show, Eq, Read, Generic)

-- All available operations in this grammar
data Operation
  = -- Int
    AddInt
  | SubInt
  | MultInt
  | DivInt
  | ModInt
  | MaxInt
  | MinInt
  | -- Bool
    And
  | Or
  | Not
  | If
  | -- Float
    AddFloat
  | SubFloat
  | MultFloat
  | DivFloat
  | Sqrt
  | -- Char + Bool
    EqChar
  | IsLetter
  | IsDigit
  | -- Float + Int
    IntToFloat
  | Floor
  | -- Int + Bool
    GtInt
  | LtInt
  | EqInt
  | -- Lists
    Len
  | Reverse
  | Singleton
  | Cons
  | Head
  | Range
  | Map
  | Filter
  | Concat
  | Zip
  | Take
  | SumFloats
  | ProductFloats
  | SumInts
  | ProductInts
  | -- String
    Unlines
  | ShowInt
  | -- Pair
    ToPair
  | Fst
  | Snd
  deriving (Show, Eq, Enum, Bounded, Ord, Read, Generic)

-- * Shorthands

-- | Shorthand for Node
(<|) :: Operation -> [Tree] -> Tree
(<|) = Node Nothing

-- | Shorthand for creating a function type
(->>) :: ArgTypes -> OutputType -> FunctionType
(->>) = MkFunctionType

-- | Shorthand for matching a literal
pattern LeafLit :: Lit -> Tree
pattern LeafLit a <- Leaf _ (Literal a) where LeafLit a = Leaf Nothing (Literal a)

-- | Shorthand for matching an argument
pattern LeafArg :: Int -> Tree
pattern LeafArg i <- Leaf _ (Arg i) where LeafArg i = Leaf Nothing (Arg i)

{-# COMPLETE LeafArg, LeafLit, Node #-}

-- | Composes a sequence of operations together
compOp :: [Operation] -> [Tree] -> Tree
compOp [x] ts = x <| ts
compOp (x : xs) ts = x <| [compOp xs ts]
compOp _ _ = error "empty list on compOp!"
