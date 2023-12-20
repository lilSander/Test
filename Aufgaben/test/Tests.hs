module Main where

import Text.Printf

import Test.Tasty
import Test.Tasty.HUnit

import Positions
import Pad
import SamePrefix
import NTree
import PadInf

main = defaultMain $ testGroup "Programmieraufgaben" [
    exercise1,
    exercise2,
    exercise3,
    exercise4,
    exercise5,
    exercise6
  ]

exercise1 = exercise "1" "Positionen" 10 False $ [
  testCase "positions \"Foo\"" $
    positions "Foo" @?= 
      [('F',1),('o',2),('o',3)]
  ]

exercise2 = exercise "2" "Listen auffüllen" 15 False $ [
  testCase "rpad '*' [\"abcde\",\"fgh\",\"uvwxyz\"]" $
    rpad '*' ["abcde","fgh","uvwxyz"] @?=
      ["abcde*","fgh***","uvwxyz"],
  testCase "rpad 0 [[1,3,5], [1..5], [7,9]]" $
    rpad 0 [[1,3,5],[1..5],[7,9]] @?=
      [[1,3,5,0,0],[1,2,3,4,5],[7,9,0,0,0]]
  ]                  

exercise3 = exercise "3" "Eigenschaften" 10 False $ [
  testCase ("samePrefix [\"a\", \"fg\", \"uvw\"] [\"abc\",\"fgh\",\"uvw\"]") $
    samePrefix ["a","fg","uvw"] ["abc","fgh","uvw"] @?= True,
  testCase ("samePrefix [[1,3],[7,10],[8]] [[1,3,5],[7,9],[8,12]]") $    
    samePrefix [[1,3],[7,10],[8]] [[1,3,5],[7,9],[8,12]] @?= False,
  testCase ("samePrefix [] []") $ samePrefix ([] :: [String]) [] @?= True,
  testCase ("samePrefix [] [\"foo\"]") $    
  samePrefix [] ["foo"] @?= False
  ] 

t1 = Node [Leaf "1", Node [], Leaf "2"]
t2 = Node [Leaf "a", Node [Leaf "b", Leaf "c",
       Node [Leaf "d", Leaf "e"], Leaf "f", Node [Leaf "x"]]]

exercise4 = exercise "4" "Variadische Bäume ..." 25 False $ [
    testGroup "... wohlgeformt" [
        testCase "wf t1" $ wf t1 @?= False,
        testCase "wf t2" $ wf t2 @?= True
      ],
    testCase "... schön ausgedruckt" $
      render t2 @?= (unlines [
        "  \"a\"",
        "    \"b\"",
        "    \"c\"",
        "      \"d\"",
        "      \"e\"",
        "    \"f\"",
        "      \"x\""          
      ]),
    testGroup "... vermessen" [
      testCase "height t1" $ height t1 @?= 2,
      testCase "height t2" $ height t2 @?= 4
    ],
    testGroup "... balanciert" [
      testCase "balanced t1" $ balanced t1 @?= True,
      testCase "balanced t2" $ balanced t2 @?= False
    ]      
  ]

exercise5 = exercise "5" "Funktorinstanz" 10 False $ [
    testCase "fmap (succ . head) t" $
      (fmap (succ . head) t2) @?= 
        (Node [Leaf 'b', Node [Leaf 'c', Leaf 'd', Node [Leaf 'e', Leaf 'f'], Leaf 'g', Node [Leaf 'y']]])
  ]

exercise6 = exercise "6" "Unendliche aufgefüllt" 5 True $ [
    testCase "map (take 5) $ rpadinf [[1,3..], [1..], [7,9]]" $
      map (take 5) (rpadinf 0 [1:3:5:7:9:undefined, 1:2:3:4:5:6:undefined, [7,9]]) @?= [[1,3,5,7,9],[1,2,3,4,5],[7,9,0,0,0]]
  ]

exercise :: String -> String -> Int -> Bool -> [TestTree] -> TestTree
exercise i n p b = testGroup (printf "%s %-40s (%d Punkte)" i n p)
  where pnts = if b then "Bonuspunkte" else "Punkte"
          

