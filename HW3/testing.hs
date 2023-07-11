{-# LANGUAGE StandaloneDeriving #-}
import Test.HUnit
import System.IO
import Control.Exception (evaluate)
import qualified Data.Set as Set
import Data.Char ( toUpper, toLower )
import Data.List (sort)
import Data.Ord
import Data.Text
import qualified HW3
import Test.HUnit (assertEqual)
import System.Timeout


testWithTimeout :: (Eq a, Show a) => Int -> a -> a -> Assertion
testWithTimeout time expected action = do
  result <- timeout time (evaluate $ action)
  case result of
    Just value -> assertEqual "" expected value
    Nothing -> assertEqual "Timeout expired" False True


removeSpaces :: String -> String
removeSpaces = unpack . strip . pack

lowercaseString :: String -> String
lowercaseString str = Prelude.map Data.Char.toLower str


testPrettyPrintBasic :: Test
testPrettyPrintBasic = 
  test
    [ "Test 1: prettyPrint (HW3.Not (HW3.Var \"a\"))" ~: testWithTimeout (1 * 1000000) "!a" (removeSpaces (HW3.prettyPrint $ HW3.Not $ HW3.Var "a"))
    , "Test 2: prettyPrint (HW3.And (HW3.Var \"a\") (HW3.Var \"b\"))" ~: testWithTimeout (1 * 1000000) "a && b" (removeSpaces (HW3.prettyPrint $ HW3.And (HW3.Var "a") (HW3.Var "b")))
    , "Test 3: prettyPrint (HW3.Or (HW3.Not (HW3.Var \"a\")) (HW3.And (HW3.Var \"b\") (HW3.Var \"c\")))" ~: testWithTimeout (1 * 1000000) "!a || (b && c)" (removeSpaces (HW3.prettyPrint $ HW3.Or (HW3.Not (HW3.Var "a")) (HW3.And (HW3.Var "b") (HW3.Var "c"))))
    , "Test 4: prettyPrint (HW3.And (HW3.Not (HW3.Or (HW3.Var \"a\") (HW3.Var \"b\"))) (HW3.Not (HW3.Var \"c\")))" ~: testWithTimeout (1 * 1000000) "!(a || b) && !c" (removeSpaces (HW3.prettyPrint $ HW3.And (HW3.Not (HW3.Or (HW3.Var "a") (HW3.Var "b"))) (HW3.Not (HW3.Var "c"))))
    , "Test 5: prettyPrint (HW3.Or (HW3.And (HW3.Var \"a\") (HW3.Var \"b\")) (HW3.And (HW3.Var \"c\") (HW3.Var \"d\")))" ~: testWithTimeout (1 * 1000000) "(a && b) || (c && d)" (removeSpaces (HW3.prettyPrint $ HW3.Or (HW3.And (HW3.Var "a") (HW3.Var "b")) (HW3.And (HW3.Var "c") (HW3.Var "d"))))
    ]

testPrettyPrintAdvance :: Test
testPrettyPrintAdvance =
  test
    [ "Test 1: prettyPrint (HW3.If (HW3.And (HW3.Var \"x\") (HW3.Not (HW3.Var \"y\"))) [(HW3.Define \"z\" (HW3.Var \"x\")), (HW3.Block [(HW3.Define \"y\" (HW3.Literal False)), (HW3.Define \"y\" (HW3.And (HW3.Var \"y\") (HW3.Var \"z\")))]), (HW3.Define \"a\" (HW3.Or (HW3.Var \"z\") (HW3.Var \"y\"))), (HW3.Block []), (HW3.IfElse (HW3.Var \"a\") [(HW3.Return (HW3.Literal True))] [(HW3.Return (HW3.Var \"z\"))])])" ~: testWithTimeout (1 * 1000000) (lowercaseString "if (x && !y) {\n  z = x\n  {\n    y = False\n    y = y && z\n  }\n  a = z || y\n  {\n  }\n  if (a) {\n    return True\n  } else {\n    return z\n  }\n}") (lowercaseString (removeSpaces (HW3.prettyPrint $ HW3.If (HW3.And (HW3.Var "x") (HW3.Not (HW3.Var "y"))) [HW3.Define "z" (HW3.Var "x"), HW3.Block [HW3.Define "y" (HW3.Literal False), HW3.Define "y" (HW3.And (HW3.Var "y") (HW3.Var "z"))], HW3.Define "a" (HW3.Or (HW3.Var "z") (HW3.Var "y")), HW3.Block [], HW3.IfElse (HW3.Var "a") [HW3.Return (HW3.Literal True)] [HW3.Return (HW3.Var "z")]])))
    , "Test 2: prettyPrint IfElse" ~: testWithTimeout (1 * 1000000) (lowercaseString "if (x || !y) {\n  return z\n} else {\n  return True\n}") (lowercaseString $ removeSpaces (HW3.prettyPrint $ HW3.IfElse (HW3.Or (HW3.Var "x") (HW3.Not (HW3.Var "y"))) [HW3.Return (HW3.Var "z")] [HW3.Return (HW3.Literal True)]))
    , "Test 3: prettyPrint Define inside Block" ~: testWithTimeout (1 * 1000000) (lowercaseString "{\n  a = x && y\n  return a\n}") (lowercaseString $ removeSpaces (HW3.prettyPrint $ HW3.Block [HW3.Define "a" (HW3.And (HW3.Var "x") (HW3.Var "y")), HW3.Return (HW3.Var "a")]))
    , "Test 4: prettyPrint complex If" ~: testWithTimeout (1 * 1000000) (lowercaseString "if ((x && y) || z) {\n  a = x || y\n  b = y && z\n}") (lowercaseString $ removeSpaces (HW3.prettyPrint $ HW3.If (HW3.Or (HW3.And (HW3.Var "x") (HW3.Var "y")) (HW3.Var "z")) [HW3.Define "a" (HW3.Or (HW3.Var "x") (HW3.Var "y")), HW3.Define "b" (HW3.And (HW3.Var "y") (HW3.Var "z"))]))
    , "Test 5: prettyPrint nested If" ~: testWithTimeout (1 * 1000000) (lowercaseString "if (x || y) {\n  if (y && z) {\n    return a\n  }\n}") (lowercaseString $ removeSpaces (HW3.prettyPrint $ HW3.If (HW3.Or (HW3.Var "x") (HW3.Var "y")) [HW3.If (HW3.And (HW3.Var "y") (HW3.Var "z")) [HW3.Return (HW3.Var "a")]]))
    , "Test 6: prettyPrint Return" ~: testWithTimeout (1 * 1000000) (lowercaseString "return x") (lowercaseString $ removeSpaces (HW3.prettyPrint $ HW3.Return (HW3.Var "x")))
    , "Test 7: prettyPrint complex IfElse" ~: testWithTimeout (1 * 1000000) (lowercaseString "if (x && y) {\n  return z\n} else {\n  return y\n}") (lowercaseString $ removeSpaces (HW3.prettyPrint $ HW3.IfElse (HW3.And (HW3.Var "x") (HW3.Var "y")) [HW3.Return (HW3.Var "z")] [HW3.Return (HW3.Var "y")]))
    , "Test 8: prettyPrint complex Block" ~: testWithTimeout (1 * 1000000) (lowercaseString "{\n  a = x || y\n  b = y && z\n  return a && b\n}") (lowercaseString $ removeSpaces (HW3.prettyPrint $ HW3.Block [HW3.Define "a" (HW3.Or (HW3.Var "x") (HW3.Var "y")), HW3.Define "b" (HW3.And (HW3.Var "y") (HW3.Var "z")), HW3.Return (HW3.And (HW3.Var "a") (HW3.Var "b"))]))
    ]

testPrettyPrintList :: Test
testPrettyPrintList =
  test
    [ "Test 1: Pretty Print multiple Statements" 
        ~: lowercaseString ("x = True\ny = False\nreturn x && y")
        ~=? lowercaseString (removeSpaces (HW3.prettyPrint 
            [ HW3.Define "x" (HW3.Literal True)
            , HW3.Define "y" (HW3.Literal False)
            , HW3.Return (HW3.And (HW3.Var "x") (HW3.Var "y"))
            ]))

    , "Test 2: Pretty Print single element list" 
        ~: lowercaseString ("z = True")
        ~=? lowercaseString (removeSpaces (HW3.prettyPrint 
            [ HW3.Define "z" (HW3.Literal True)]))
    ]


testSimplify :: Test
testSimplify =
  test
    [ "Test 1: simplify Var with known value" ~: testWithTimeout (1 * 1000000) [HW3.Define "x" (HW3.Literal True),HW3.Return (HW3.Literal True)] (HW3.simplify [HW3.Define "x" (HW3.Literal True), HW3.Return (HW3.Var "x")])
    , "Test 2: simplify Not of a Literal" ~: testWithTimeout (1 * 1000000) [HW3.Return (HW3.Literal False)] (HW3.simplify [HW3.Return (HW3.Not (HW3.Literal True))])
    , "Test 3: simplify Or with a true operand" ~: testWithTimeout (1 * 1000000) [HW3.Return (HW3.Literal True)] (HW3.simplify [HW3.Return (HW3.Or (HW3.Var "x") (HW3.Literal True))])
    , "Test 4: simplify And with a false operand" ~: testWithTimeout (1 * 1000000) [HW3.Return (HW3.Literal False)] (HW3.simplify [HW3.Return (HW3.And (HW3.Var "x") (HW3.Literal False))])
    , "Test 5: simplify If with a true condition" ~: testWithTimeout (1 * 1000000) [HW3.Block [HW3.Return (HW3.Var "x")]] (HW3.simplify [HW3.If (HW3.Literal True) [HW3.Return (HW3.Var "x")]])
    , "Test 6: simplify IfElse with a false condition" ~: testWithTimeout (1 * 1000000) [HW3.Block [HW3.Return (HW3.Var "y")]] (HW3.simplify [HW3.IfElse (HW3.Literal False) [HW3.Return (HW3.Var "x")] [HW3.Return (HW3.Var "y")]])
    , "Test 7: simplify with variable definition scope" ~: testWithTimeout (1 * 1000000) [HW3.Define "x" (HW3.Literal True), HW3.Block [HW3.Return (HW3.Literal True)]] (HW3.simplify [HW3.Define "x" (HW3.Literal True), HW3.Block [HW3.Return (HW3.Var "x")]])
    , "Test 8: simplify nested blocks with variable definition scope" ~: testWithTimeout (1 * 1000000) [HW3.Block [HW3.Define "x" (HW3.Literal True), HW3.Block [HW3.Define "x" (HW3.Literal False), HW3.Return (HW3.Literal False)]]] (HW3.simplify [HW3.Block [HW3.Define "x" (HW3.Literal True), HW3.Block [HW3.Define "x" (HW3.Literal False), HW3.Return (HW3.Var "x")]]])
    , "Test 9: Simplify if (x) { y = x } to if (x) { y = True }" ~: testWithTimeout (1 * 1000000) [HW3.If (HW3.Var "x") [HW3.Block [HW3.Define "y" (HW3.Literal True)]]] (HW3.simplify [HW3.If (HW3.Var "x") [HW3.Block [HW3.Define "y" (HW3.Var "x")]]])
    , "Test 10: Simplify expression with few rules" 
        ~: testWithTimeout (1 * 1000000) 
            [HW3.Define "y" (HW3.Literal False),HW3.Block [HW3.Return (HW3.Literal False)]]
            (HW3.simplify [
              HW3.Define "y" (HW3.Literal False),
              HW3.If (HW3.Or (HW3.Not (HW3.Var "y")) (HW3.Var "x"))
                [
                  HW3.Return (HW3.And (HW3.Var "y") (HW3.Not (HW3.Var "y")))
                ]
            ]
            )
    , "Test 11: Simplify expression with nested if-else structures" 
    ~: testWithTimeout (1 * 1000000) 
        [HW3.Define "y" (HW3.Literal False),HW3.Define "z" (HW3.Literal False),HW3.Block [HW3.Return (HW3.Literal False)]]
        (HW3.simplify 
            [
              HW3.Define "y" (HW3.Literal False),
              HW3.Define "z" (HW3.Literal False),
              HW3.IfElse (HW3.Or (HW3.Not (HW3.Var "y")) (HW3.Var "z"))
                [
                  HW3.Return (HW3.And (HW3.Var "y") (HW3.Not (HW3.Var "y")))
                ]
                [
                  HW3.Define "z" (HW3.Literal True),
                  HW3.Return (HW3.And (HW3.Var "z") (HW3.Not (HW3.Var "y")))
                ]
            ]
        )
    , "Test 12: Simplify expression with compound logical operations" 
    ~: testWithTimeout (1 * 1000000) 
        [HW3.Define "y" (HW3.Literal False)]
        (HW3.simplify 
            [
              HW3.Define "y" (HW3.Literal False),
              HW3.If (HW3.And (HW3.Var "y") (HW3.Or (HW3.Not (HW3.Var "y")) (HW3.Var "y")))
                [
                  HW3.Return (HW3.And (HW3.Var "y") (HW3.Not (HW3.Var "y")))
                ]
            ]
        )


    ]


testTrees :: Test
testTrees =
  test
    [
        "Test 1: Check inequality of trees" 
          ~: False
          ~=? (HW3.Tree (HW3.Empty) 4 (HW3.Tree (HW3.Empty) 5 (HW3.Tree (HW3.Empty) 6 (HW3.Empty)))) == (HW3.Tree (HW3.Tree (HW3.Empty) 4 (HW3.Empty)) 5 (HW3.Empty))
      , "Test 2: Check show implementation" 
          ~: "{1,2,3,4,5,6}" 
          ~=? show (HW3.Tree (HW3.Tree (HW3.Tree (HW3.Empty) 1 (HW3.Empty)) 2 (HW3.Tree (HW3.Empty) 3 (HW3.Empty))) 4 (HW3.Tree (HW3.Tree (HW3.Empty) 5 (HW3.Empty)) 6 HW3.Empty))
      , "Test 3: Check comparison of unequal trees (greater)" 
        ~: GT 
        ~=? compare (HW3.Tree (HW3.Empty) 4 (HW3.Tree (HW3.Empty) 5 (HW3.Tree (HW3.Empty) 7 (HW3.Empty)))) (HW3.Tree (HW3.Tree (HW3.Empty) 4 (HW3.Empty)) 5 (HW3.Tree (HW3.Empty) 6 HW3.Empty))
      , "Test 4: Check comparison of unequal trees (less)" 
        ~: LT 
        ~=? compare (HW3.Tree (HW3.Empty) 4 (HW3.Tree (HW3.Empty) 5 (HW3.Tree (HW3.Empty) 6 (HW3.Empty)))) (HW3.Tree (HW3.Empty) 4 (HW3.Tree (HW3.Empty) 5 (HW3.Tree (HW3.Empty) 7 (HW3.Empty))))
      , "Test 5: Check comparison of differently structured, but equivalent trees with binary tree"
        ~: EQ 
        ~=? compare (HW3.Tree (HW3.Tree (HW3.Empty) 4 (HW3.Tree (HW3.Empty) 5 (HW3.Tree (HW3.Empty) 6 (HW3.Empty)))) 7 (HW3.Tree (HW3.Empty) 8 (HW3.Tree (HW3.Empty) 9 (HW3.Empty)))) (HW3.Tree (HW3.Tree (HW3.Empty) 4 (HW3.Tree (HW3.Empty) 5 (HW3.Tree (HW3.Empty) 6 (HW3.Empty)))) 7 (HW3.Tree (HW3.Empty) 8 (HW3.Tree (HW3.Empty) 9 (HW3.Empty))))
      , "Test 6: Check equality of large trees"
          ~: True
          ~=? ((HW3.Tree (HW3.Tree HW3.Empty 1 (HW3.Tree HW3.Empty 2 (HW3.Tree HW3.Empty 3 HW3.Empty))) 4 (HW3.Tree HW3.Empty 5 (HW3.Tree HW3.Empty 6 HW3.Empty))))
          == (HW3.Tree (HW3.Tree HW3.Empty 1 (HW3.Tree HW3.Empty 2 (HW3.Tree HW3.Empty 3 HW3.Empty))) 4 (HW3.Tree HW3.Empty 5 (HW3.Tree HW3.Empty 6 HW3.Empty)))
    ]

testNub :: Test
testNub =
  test
    [ "Test 1: nub on list with duplicates" 
        ~: [2,1,3,4] 
        ~=? HW3.nub [2,1,3,3,4,1,2]
    , "Test 2: nub on list with no duplicates" 
        ~: [1,2,3,4,5] 
        ~=? HW3.nub [1,2,3,4,5]
    , "Test 3: nub on empty list" 
        ~: ([] :: [Int]) 
        ~=? HW3.nub []
    , "Test 4: nub on list with all elements the same" 
        ~: [1] 
        ~=? HW3.nub [1,1,1,1,1,1]
    , "Test 5: nub on list with character duplicates" 
        ~: ['a','b','c'] 
        ~=? HW3.nub ['a','b','a','c','b','a']
    , "Test 6: nub on list with string duplicates" 
        ~: ["hello", "world", "foo"] 
        ~=? HW3.nub ["hello", "world", "hello", "foo", "world"]
    , "Test 7: nub on list with boolean duplicates" 
        ~: [True, False] 
        ~=? HW3.nub [True, False, True, True, False]
    ]


data Animal = Animal String Int deriving (Show, Eq, Ord)

animalAge :: Animal -> Int
animalAge (Animal _ a) = a

animalType :: Animal -> String
animalType (Animal t _) = t


testSort :: Test
testSort =
  test
    [ "Test 1: sort on list with duplicate integers" 
        ~: [1, 2, 2, 3, 4] 
        ~=? sort [2, 1, 3, 2, 4]
    , "Test 2: sort on list with characters" 
        ~: ['a', 'b', 'c', 'd', 'e'] 
        ~=? sort ['e', 'a', 'b', 'c', 'd']
    ]


testSortOn :: Test
testSortOn =
  test
    [ "Test 1: sortOn animalType on list of Animal objects" 
        ~: [Animal "Cat" 5, Animal "Dog" 7, Animal "Elephant" 20] 
        ~=? HW3.sortOn animalType [Animal "Elephant" 20, Animal "Cat" 5, Animal "Dog" 7]
    , "Test 2: sortOn animalAge on list of Animal objects" 
        ~: [Animal "Cat" 5, Animal "Dog" 7, Animal "Elephant" 20] 
        ~=? HW3.sortOn animalAge [Animal "Elephant" 20, Animal "Cat" 5, Animal "Dog" 7]
    , "Test 3: sortOn (Down . animalType) on list of Animal objects" 
        ~: [Animal "Elephant" 20, Animal "Dog" 7, Animal "Cat" 5] 
        ~=? HW3.sortOn (Down . animalType) [Animal "Elephant" 20, Animal "Cat" 5, Animal "Dog" 7]
    , "Test 4: sortOn (Down . animalAge) on list of Animal objects" 
        ~: [Animal "Elephant" 20, Animal "Dog" 7, Animal "Cat" 5] 
        ~=? HW3.sortOn (Down . animalAge) [Animal "Elephant" 20, Animal "Cat" 5, Animal "Dog" 7]
    ]


allTests :: Test
allTests =
  test
    [ 
      "testPrettyPrintBasic" ~: testPrettyPrintBasic
      ,"testPrettyPrintAdvance" ~: testPrettyPrintAdvance
      ,"testPrettyPrintList" ~: testPrettyPrintList
      ,"testSimplify" ~: testSimplify
      ,"testTrees" ~: testTrees
      ,"testNub" ~: testNub
      ,"testSort" ~: testSort
      ,"testSortOn" ~: testSortOn
    ] 

-- Run the tests and output the results to a file
main :: IO ()
main = do
  let outputFile = "test_results.txt"
  (counts, failures) <- runTestText (putTextToHandle stdout False) allTests
  putStrLn $ ""
