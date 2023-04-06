module ProblemTrees where

import Grammar

numberIOTree :: Tree
numberIOTree = AddFloat <| [IntToFloat <| [arg 0], arg 1]

smallOrLargeTree :: Tree
smallOrLargeTree =
  If
    <| [ LtInt <| [arg 0, iLitT 1000],
         stLitT "small",
         If <| [LtInt <| [arg 0, iLitT 2000], stLitT "", stLitT "large"]
       ]

forLoopIndexTree :: Tree
forLoopIndexTree = Unlines <| [Map <| [lambdaLitT (ShowInt <| [arg 0]) ([GInt] ->> GList GChar), Range <| [arg 0, arg 1, arg 2]]]

compareStringLengthsTree :: Tree
compareStringLengthsTree =
  And
    <| [ LtInt <| [Len <| [arg 0], Len <| [arg 1]],
         LtInt <| [Len <| [arg 1], Len <| [arg 2]]
       ]

doubleLettersTree :: Tree
doubleLettersTree =
  Concat
    <| [ Map
           <| [ lambdaLitT lambda ([GChar] ->> GList GChar),
                arg 0
              ]
       ]
  where
    lambda :: Tree
    lambda =
      If
        <| [ EqChar <| [arg 0, chLitT '!'],
             stLitT "!!!",
             If
               <| [ IsLetter <| [arg 0],
                    listConsNode GChar [arg 0, arg 0],
                    listConsNode GChar [arg 0]
                  ]
           ]

replaceSpaceWithNewlineTree :: Tree
replaceSpaceWithNewlineTree = ToPair <| [str, count]
  where
    str = Map <| [lambdaLitT lambdaMap ([GChar] ->> GChar), arg 0]
    count = Len <| [Filter <| [lambdaLitT lambdaFilter ([GChar] ->> GBool), arg 0]]

    lambdaMap :: Tree
    lambdaMap =
      If
        <| [ EqChar <| [arg 0, chLitT ' '],
             chLitT '\n',
             arg 0
           ]

    lambdaFilter :: Tree
    lambdaFilter = Not <| [EqChar <| [arg 0, chLitT ' ']]

stringDifferencesTree :: Tree
stringDifferencesTree =
  Filter
    <| [ lambdaLitT lambdaFilter ([GPair GInt (GPair GChar GChar)] ->> GBool),
         Zip <| [Range <| [iLitT 0, Len <| [arg 0], iLitT 1], Zip <| [arg 0, arg 1]]
       ]
  where
    lambdaFilter :: Tree
    lambdaFilter = Not <| [EqChar <| [Fst <| [Snd <| [arg 0]], Snd <| [Snd <| [arg 0]]]]

evenSquaresTree :: Tree
evenSquaresTree =
  Filter
    <| [ lambdaLitT evenTree ([GInt] ->> GBool),
         Map
           <| [ lambdaLitT squaredTree ([GInt] ->> GInt),
                Range <| [iLitT 0, compOp [Floor, Sqrt, IntToFloat] [arg 0], iLitT 1]
              ]
       ]
  where
    squaredTree :: Tree
    squaredTree = MultInt <| [arg 0, arg 0]

    evenTree :: Tree
    evenTree = EqInt <| [iLitT 0, ModInt <| [arg 0, iLitT 2]]

wallisPiTree :: Tree
wallisPiTree = ProductFloats <| [Map <| [lambdaLitT mapTree ([GPair GInt GInt] ->> GFloat), Take <| [arg 0, listTree]]]
  where
    listTree :: Tree
    listTree = Zip <| [top, bot]
    top, bot :: Tree
    top = Cons <| [iLitT 2, dup $ Range <| [iLitT 4, AddInt <| [MultInt <| [iLitT 2, DivInt <| [arg 0, iLitT 2]], iLitT 2], iLitT 2]]
    bot = dup $ Range <| [iLitT 3, AddInt <| [MultInt <| [iLitT 2, DivInt <| [AddInt <| [iLitT 1, arg 0], iLitT 2]], iLitT 1], iLitT 2]

    dup :: Tree -> Tree
    dup x = Concat <| [Map <| [lambdaLitT (listConsNode GInt [arg 0, arg 0]) ([GInt] ->> GList GInt), x]]

    mapTree :: Tree
    mapTree = DivFloat <| [IntToFloat <| [Fst <| [arg 0]], IntToFloat <| [Snd <| [arg 0]]]

stringLengthsBackwardsTree :: Tree
stringLengthsBackwardsTree = Reverse <| [Map <| [lambdaLitT (Len <| [arg 0]) ([GList GChar] ->> GInt), arg 0]]

lastIndexOfZeroTree :: Tree
lastIndexOfZeroTree =
  compOp
    [Fst, Head, Reverse, Filter]
    [lambdaLitT lambda ([GPair GInt GInt] ->> GBool), Zip <| [Range <| [iLitT 0, Len <| [arg 0], iLitT 1], arg 0]]
  where
    lambda :: Tree
    lambda = EqInt <| [iLitT 0, Snd <| [arg 0]]

vectorAverageTree :: Tree
vectorAverageTree = DivFloat <| [SumFloats <| [arg 0], IntToFloat <| [Len <| [arg 0]]]

countOddsTree :: Tree
countOddsTree = Len <| [Filter <| [lambdaLitT lambda ([GList GFloat] ->> GInt), arg 0]]
  where
    lambda :: Tree
    lambda = EqInt <| [iLitT 1, ModInt <| [arg 0, iLitT 2]]

mirrorImageTree :: Tree
mirrorImageTree =
  EqInt <| [Len <| [arg 0], Len <| [filtered]]
  where
    filtered :: Tree
    filtered =
      Filter
        <| [ lambdaLitT (EqInt <| [Fst <| [arg 0], Snd <| [arg 0]]) ([GPair GInt GInt] ->> GBool),
             Zip <| [arg 0, Reverse <| [arg 1]]
           ]

sumOfSquaresTree :: Tree
sumOfSquaresTree = SumInts <| [Map <| [lambdaLitT (MultInt <| [arg 0, arg 0]) ([GInt] ->> GInt), Range <| [iLitT 1, arg 0, iLitT 1]]]

vectorsSummedTree :: Tree
vectorsSummedTree = Map <| [lambdaLitT (AddInt <| [Fst <| [arg 0], Snd <| [arg 0]]) ([GPair GInt GInt] ->> GInt), Zip <| [arg 0, arg 1]]

negativeToZeroTree :: Tree
negativeToZeroTree = Map <| [lambdaLitT (If <| [LtInt <| [arg 0, iLitT 0], iLitT 0, arg 0]) ([GList GInt] ->> GInt), arg 0]

gradeTree :: Tree
gradeTree = Concat <| [templateStr]
  where
    templateStr = Cons <| [stLitT "Student has a ", Cons <| [checkF, Singleton <| [stLitT " grade."]]]
    checkF :: Tree
    checkF = If <| [LtInt <| [arg 4, arg 3], stLitT "F", checkD]
    checkD :: Tree
    checkD = If <| [LtInt <| [arg 4, arg 2], stLitT "D", checkC]
    checkC :: Tree
    checkC = If <| [LtInt <| [arg 4, arg 1], stLitT "C", checkB]
    checkB :: Tree
    checkB = If <| [LtInt <| [arg 4, arg 0], stLitT "B", stLitT "A"]

-- min (max (min x0 x1) x2) (max x0 x1)
medianTree :: Tree
medianTree = MinInt <| [MaxInt <| [MinInt <| [arg 0, arg 1], arg 2], MaxInt <| [arg 0, arg 1]]

smallestTree :: Tree
smallestTree = MinInt <| [MinInt <| [arg 0, arg 1], MinInt <| [arg 2, arg 3]]