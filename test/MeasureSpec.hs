module MeasureSpec where

import Data.Maybe (fromJust)
import Grammar
import Grammar.Helpers (computeMeasure)
import Pretty
import ProblemTrees
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testCase "Leaf Arg" $
      getMeasure (LeafArg 0)
        @?= ( MkMeasure
                { _currentDepth = 0,
                  _height = 0,
                  _nodeCount = 1
                }
            ),
    testCase "Leaf Lit" $
      getMeasure (iLitT 0)
        @?= ( MkMeasure
                { _currentDepth = 0,
                  _height = 0,
                  _nodeCount = 1
                }
            ),
    testCase "Number IO" $
      getMeasure numberIOTree
        @?= ( MkMeasure
                { _currentDepth = 0,
                  _height = 2,
                  _nodeCount = 4
                }
            ),
    testCase "Small Or Large" $
      getMeasure smallOrLargeTree
        @?= ( MkMeasure
                { _currentDepth = 0,
                  _height = 3,
                  _nodeCount = 11
                }
            )
  ]
