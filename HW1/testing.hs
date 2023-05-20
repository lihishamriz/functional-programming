{-# LANGUAGE StandaloneDeriving #-}
import Test.HUnit
import System.IO
import Control.Exception (evaluate)
import qualified Data.Set as Set
import Data.Char ( toUpper, toLower )
import qualified HW1
import Test.HUnit (assertEqual)
import System.Timeout
import HW1 (Error(MissingVar))


-- Test cases
testFromMaybe :: Test
testFromMaybe =
  test
    [ "Test 1: fromMaybe 0 (Just 42)" ~: HW1.fromMaybe 0 (Just 42) ~?= 42
    , "Test 2: fromMaybe \"default\" (Just \"Hello\")" ~: HW1.fromMaybe "default" (Just "Hello") ~?= "Hello"
    , "Test 3: fromMaybe True Nothing" ~: HW1.fromMaybe True Nothing ~?= True
    , "Test 4: fromMaybe 10 Nothing" ~: HW1.fromMaybe 10 Nothing ~?= 10
    ]

testMaybe :: Test
testMaybe =
  test
    [ "Test 1: maybe 0 (+ 1) (Just 42)" ~: HW1.maybe 0 (+ 1) (Just 42) ~?= 43
    , "Test 2: maybe \"default\" (++ \" World\") (Just \"Hello\")" ~: HW1.maybe "default" (++ " World") (Just "Hello") ~?= "Hello World"
    , "Test 3: maybe True not Nothing" ~: HW1.maybe True not Nothing ~?= True
    , "Test 4: maybe 10 (* 2) Nothing" ~: HW1.maybe 10 (* 2) Nothing ~?= 10
    , "Test 5: maybe 0 (* 2) (Just 5)" ~: HW1.maybe 0 (* 2) (Just 5) ~?= 10
    , "Test 6: maybe [] (\\x -> [x, x + 1]) (Just 3)" ~: HW1.maybe [] (\x -> [x, x + 1]) (Just 3) ~?= [3, 4]
    , "Test 7: maybe 'a' toUpper Nothing" ~: HW1.maybe 'a' toUpper Nothing ~?= 'a'
    ]

testCatMaybes :: Test
testCatMaybes =
  test
    [ "Test 1: catMaybes [Just 1, Nothing, Just 2, Just 3]" ~: HW1.catMaybes [Just 1, Nothing, Just 2, Just 3] ~?= [1, 2, 3]
    , "Test 2: catMaybes [Just \"Hello\", Nothing, Just \"World\"]" ~: HW1.catMaybes [Just "Hello", Nothing, Just "World"] ~?= ["Hello", "World"]
    , "Test 3: catMaybes ([] :: [Maybe Int])" ~: HW1.catMaybes ([] :: [Maybe Int]) ~?= []
    ]

testMapMaybe :: Test
testMapMaybe =
  test
    [ "Test 1: mapMaybe (\\x -> if even x then Just (x * x) else Nothing) [1..10]" ~: HW1.mapMaybe (\x -> if even x then Just (x * x) else Nothing) [1..10] ~?= [4, 16, 36, 64, 100]
    , "Test 2: mapMaybe (\\x -> if x > 0 then Just (x * 2) else Nothing) [-3..3]" ~: HW1.mapMaybe (\x -> if x > 0 then Just (x * 2) else Nothing) [-3..3] ~?= [2, 4, 6]
    , "Test 3: mapMaybe (\\x -> if x < 0 then Just (x * x) else Nothing) [-5..5]" ~: HW1.mapMaybe (\x -> if x < 0 then Just (x * x) else Nothing) [-5..5] ~?= [25, 16, 9, 4, 1]
    , "Test 4: mapMaybe (\\x -> if x == 'a' then Just 'A' else Nothing) \"abcd\"" ~: HW1.mapMaybe (\x -> if x == 'a' then Just 'A' else Nothing) "abcd" ~?= "A"
    ]

testEither :: Test
testEither =
  test
    [ "Test 1: either (+ 1) (+ 2) (Left 5)" ~: HW1.either (+ 1) (+ 2) (Left 5) ~?= 6
    , "Test 2: either (+ 1) (+ 2) (Right 5)" ~: HW1.either (+ 1) (+ 2) (Right 5) ~?= 7
    , "Test 3: either id (\\x -> x ++ x) (Left \"Hello\")" ~: HW1.either id (\x -> x ++ x) (Left "Hello") ~?= "Hello"
    , "Test 4: either id (\\x -> x ++ x) (Right \"World\")" ~: HW1.either id (\x -> x ++ x) (Right "World") ~?= "WorldWorld"
    ]

testMapLeft :: Test
testMapLeft =
  test
  [ 
    "Test 1: mapLeft (+ 1) (Left 5)" ~: HW1.mapLeft (+ 1) (Left 5) ~?= (Left 6 :: Either Int Int)
  , "Test 2: mapLeft (* 2) (Right 5)" ~: HW1.mapLeft (* 2) (Right 5) ~?= (Right 5 :: Either Int Int)
  , "Test 3: mapLeft show (Left True)" ~: HW1.mapLeft show (Left True) ~?= (Left "True" :: Either String Bool)
  , "Test 4: mapLeft id (Left [1, 2, 3])" ~: HW1.mapLeft id (Left [1, 2, 3]) ~?= (Left [1, 2, 3] :: Either [Int] Bool)
  , "Test 5: mapLeft (\\x -> x ++ x) (Right \"World\")" ~: HW1.mapLeft (\x -> x ++ x) (Right "World") ~?= (Right "World" :: Either String String)
  ]



testCatEithers :: Test
testCatEithers =
  test
    [ "Test 1: catEithers [Left \"Hello\", Right 42, Left \"World\"]" ~: HW1.catEithers [Left "Hello", Right 42, Left "World"] ~?= Left "Hello"
    , "Test 2: catEithers [Right \"Foo\", Left 123, Right \"Bar\"]" ~: HW1.catEithers [Right "Foo", Left 123, Right "Bar"] ~?= Left 123
    , "Test 3: catEithers [Left True, Left False, Right \"Baz\"]" ~: HW1.catEithers [Left True, Left False, Right "Baz"] ~?= Left True
    , "Test 4: catEithers [Right \"Qux\", Right \"Quux\", Right \"Corge\"]" ~: HW1.catEithers [Right "Qux", Right "Quux", Right "Corge"] ~?= (Right ["Qux", "Quux", "Corge"] :: Either Int [String])
    , "Test 5: catEithers []" ~: HW1.catEithers [] ~?= (Right [] :: Either Int [Int])
    , "Test 6: catEithers [Right 1, Right 2, Right 3, Right 4]" ~: HW1.catEithers [Right 1, Right 2, Right 3, Right 4] ~?= (Right [1, 2, 3, 4] :: Either Int [Int])
    ]

testMapEither :: Test
testMapEither =
  test
    [ "Test 1: mapEither (\\x -> if x > 0 then Right $ x * 10 else Left $ x + 5) [1, 2, 3]" ~: HW1.mapEither (\x -> if x > 0 then Right $ x * 10 else Left $ x + 5) [1, 2, 3] ~?= Right [10, 20, 30]
    , "Test 2: mapEither (\\x -> if x > 0 then Right $ x * 10 else Left $ x + 5) [1, -1, 2, -2]" ~: HW1.mapEither (\x -> if x > 0 then Right $ x * 10 else Left $ x + 5) [1, -1, 2, -2] ~?= Left 4
    , "Test 3: mapEither (\\x -> if even x then Right (x `div` 2) else Left (x * 2)) [2, 4, 6, 8]" ~: HW1.mapEither (\x -> if even x then Right (x `div` 2) else Left (x * 2)) [2, 4, 6, 8] ~?= Right [1, 2, 3, 4]
    , "Test 4: mapEither (\\x -> if even x then Right (x `div` 2) else Left (x * 2)) [1, 2, 4, 6]" ~: HW1.mapEither (\x -> if even x then Right (x `div` 2) else Left (x * 2)) [1, 2, 4, 6] ~?= Left 2
    , "Test 5: mapEither (\\x -> if x > 5 then Right (x - 5) else Left (5 - x)) [1..10]" ~: HW1.mapEither (\x -> if x > 5 then Right (x - 5) else Left (5 - x)) [1..10] ~?= Left 4
    , "Test 6: mapEither (\\x -> if x > 5 then Right (x - 5) else Left (5 - x)) []" ~: HW1.mapEither (\x -> if x > 5 then Right (x - 5) else Left (5 - x)) [] ~?= Right []
    ]

testPartitionEithers :: Test
testPartitionEithers =
  test
    [ "Test 1: partitionEithers [Left 1, Right 'a', Right 'b', Left 2, Right 'c']" ~: HW1.partitionEithers [Left 1, Right 'a', Right 'b', Left 2, Right 'c'] ~?= ([1, 2], "abc")
    , "Test 2: partitionEithers [Right \"Hello\", Left \"World\", Right \"!\" , Left \"!\"]" ~: HW1.partitionEithers [Right "Hello", Left "World", Right "!" , Left "!"] ~?= (["World", "!"], ["Hello", "!"])
    , "Test 3: partitionEithers [Left \"Hello\", Right 42, Left \"World\", Right 84]" ~: HW1.partitionEithers [Left "Hello", Right 42, Left "World", Right 84] ~?= (["Hello", "World"], [42, 84])
    ]


testSnoc :: Test
testSnoc =
  test
  [ "Test 1: snoc [1, 2, 3] 4" ~: HW1.snoc [1, 2, 3] 4 ~?= [1, 2, 3, 4]
  , "Test 2: snoc [] 1" ~: HW1.snoc [] 1 ~?= [1]
  , "Test 3: snoc \"abc\" 'd'" ~: HW1.snoc "abc" 'd' ~?= "abcd"
  ]


testZipWith :: Test
testZipWith =
  test
    [ "Test 1: zipWith (+) [1, 2, 3] [4, 5, 6]" ~: HW1.zipWith (+) [1, 2, 3] [4, 5, 6] ~?= [5, 7, 9]
    , "Test 2: zipWith (*) [1, 2, 3] [4]" ~: HW1.zipWith (*) [1, 2, 3] [4] ~?= [4]
    , "Test 3: zipWith (++) [\"Hello\", \"World\"] [\"!\", \"?\"]" ~: HW1.zipWith (++) ["Hello", "World"] ["!", "?"] ~?= ["Hello!", "World?"]
    , "Test 4: zipWith (\\x y -> x + y + 1) [1, 2] [4, 5, 6]" ~: HW1.zipWith (\x y -> x + y + 1) [1, 2] [4, 5, 6] ~?= [6, 8]
    , "Test 5: zipWith (\\x y -> (x, y)) [1, 2, 3] ['a', 'b', 'c']" ~: HW1.zipWith (\x y -> (x, y)) [1, 2, 3] ['a', 'b', 'c'] ~?= [(1, 'a'), (2, 'b'), (3, 'c')]
    ]


testZip :: Test
testZip =
  test
    [ "Test 1: zip [1, 2, 3] [4, 5, 6]" ~: HW1.zip [1, 2, 3] [4, 5, 6] ~?= [(1, 4), (2, 5), (3, 6)]
    , "Test 2: zip [1, 2, 3] [4, 5]" ~: HW1.zip [1, 2, 3] [4, 5] ~?= [(1, 4), (2, 5)]
    , "Test 3: zip [1, 2] [4, 5, 6]" ~: HW1.zip [1, 2] [4, 5, 6] ~?= [(1, 4), (2, 5)]
    , "Test 4: zip ([] :: [Int]) [4, 5, 6]" ~: HW1.zip ([] :: [Int]) [4, 5, 6] ~?= []
    , "Test 5: zip [1, 2, 3] ([] :: [Int])" ~: HW1.zip [1, 2, 3] ([] :: [Int]) ~?= []
    , "Test 6: zip ([] :: [Int]) ([] :: [String])" ~: HW1.zip ([] :: [Int]) ([] :: [String]) ~?= []
    ]

testUnzip :: Test
testUnzip =
  test
    [ "Test 1: unzip [(1, 'a'), (2, 'b'), (3, 'c')]" ~: HW1.unzip [(1, 'a'), (2, 'b'), (3, 'c')] ~?= ([1, 2, 3], "abc")
    , "Test 2: unzip [(10, True), (20, False), (30, True)]" ~: HW1.unzip [(10, True), (20, False), (30, True)] ~?= ([10, 20, 30], [True, False, True])
    , "Test 3: unzip [([] :: [Int], [] :: [Int])]" ~: HW1.unzip [([] :: [Int], [] :: [Int])] ~?= ([[]], [[]])
    ]

testSplitOn :: Test
testSplitOn =
  test
    [ "Test 1: splitOn ',' \"Hello,World\"" ~: HW1.splitOn ',' "Hello,World" ~?= Just("Hello", "World")
    , "Test 2: splitOn ' ' \"This is a sentence\"" ~: HW1.splitOn ' ' "This is a sentence" ~?= Just("This", "is a sentence")
    , "Test 3: splitOn 'x' \"x\"" ~: HW1.splitOn 'x' "x" ~?= Just("", "")
    , "Test 4: splitOn ' ' \"lkaljsd\"" ~: HW1.splitOn ' ' "lkaljsd" ~?= Nothing
    ]

testParseTemplate :: Test
testParseTemplate =
  test
    [ "Test 1: parseTemplate \"Hello, ${name}!\"" ~: do
        result <- timeout (3 * 1000000) (evaluate $ HW1.parseTemplate "Hello, ${name}!")
        assertEqual "Expected parsed template" (Just (Just [HW1.PlainString "Hello, ", HW1.Variable "name", HW1.PlainString "!"])) result
    , "Test 2: parseTemplate \"My favorite numbers are ${num1} and ${num2}.\"" ~: do
        result <- timeout (3 * 1000000) (evaluate $ HW1.parseTemplate "My favorite numbers are ${num1} and ${num2}.")
        assertEqual "Expected parsed template" (Just (Just [HW1.PlainString "My favorite numbers are ", HW1.Variable "num1", HW1.PlainString " and ", HW1.Variable "num2", HW1.PlainString "."])) result
    , "Test 3: parseTemplate \"${x}\"" ~: do
        result <- timeout (3 * 1000000) (evaluate $ HW1.parseTemplate "${x}")
        assertEqual "Expected parsed template" (Just (Just [HW1.Variable "x"])) result
    , "Test 4: parseTemplate \"Hello, }\"" ~: do
        result <- timeout (3 * 1000000) (evaluate $ HW1.parseTemplate "Hello, }")
        assertEqual "Expected Nothing" (Just Nothing) result
    , "Test 5: parseTemplate \"Hello, ${x\"" ~: do
        result <- timeout (3 * 1000000) (evaluate $ HW1.parseTemplate "Hello, ${x")
        assertEqual "Expected Nothing" (Just Nothing) result
    ]

testAssignTemplate :: Test
testAssignTemplate =
  test
    [ "Test 1: assignTemplate [(\"name\", \"Alice\")] [HW1.Variable \"name\"]" ~: do
        result <- timeout (3 * 1000000) (evaluate $ HW1.assignTemplate [("name", "Alice")] [HW1.Variable "name"])
        assertEqual "Expected assignment" (Just (Right "Alice")) result
    , "Test 2: assignTemplate [(\"num1\", \"42\"), (\"num2\", \"7\")] [HW1.Variable \"num1\", HW1.PlainString \" and \", HW1.Variable \"num2\"]" ~: do
        result <- timeout (3 * 1000000) (evaluate $ HW1.assignTemplate [("num1", "42"), ("num2", "7")] [HW1.Variable "num1", HW1.PlainString " and ", HW1.Variable "num2"])
        assertEqual "Expected assignment" (Just (Right "42 and 7")) result
    , "Test 3: assignTemplate [] [HW1.Variable \"name\"]" ~: do
        result <- timeout (3 * 1000000) (evaluate $ HW1.assignTemplate [] [HW1.Variable "name"])
        assertEqual "Expected error" (Just (Left "name")) result
    , "Test 4: assignTemplate [(\"name\", \"Alice\"), (\"age\", \"30\")] [HW1.Variable \"name\"]" ~: do
        result <- timeout (3 * 1000000) (evaluate $ HW1.assignTemplate [("name", "Alice"), ("age", "30")] [HW1.Variable "name"])
        assertEqual "Expected assignment" (Just (Right "Alice")) result
    , "Test 5: assignTemplate [(\"name\", \"Bob\"), (\"amount\", \"100\")] [HW1.Variable \"name\", HW1.Variable \"amount\"]" ~: do
        result <- timeout (3 * 1000000) (evaluate $ HW1.assignTemplate [("name", "Bob"), ("amount", "100")] [HW1.Variable "name", HW1.Variable "amount"])
        assertEqual "Expected assignment" (Just (Right "Bob100")) result
    , "Test 6: assignTemplate [(\"NAME\", \"Alice\")] [HW1.Variable \"name\"]" ~: do
        result <- timeout (3 * 1000000) (evaluate $ HW1.assignTemplate [("NAME", "Alice")] [HW1.Variable "name"])
        assertEqual "Expected error" (Just (Left "name")) result
    , "Test 7: assignTemplate [(\"NAME\", \"Alice\")] [HW1.Variable \"name\", HW1.Variable \"num1\"]" ~: do
        result <- timeout (3 * 1000000) (evaluate $ HW1.assignTemplate [("NAME", "Alice")] [HW1.Variable "name", HW1.Variable "num1"])
        assertEqual "Expected error" (Just (Left "name")) result
    , "Test 8: assignTemplate [(\"name\", \"Alice\")] [HW1.Variable \"name\", HW1.PlainString \" \", HW1.Variable \"name\"]" ~: do
        result <- timeout (3 * 1000000) (evaluate $ HW1.assignTemplate [("name", "Alice")] [HW1.Variable "name", HW1.PlainString " ", HW1.Variable "name"])
        assertEqual "Expected assignment" (Just (Right "Alice Alice")) result
    ]

deriving instance Eq HW1.ParsedString
deriving instance Eq HW1.Error


testInterpolateString :: Test
testInterpolateString =
  test
    [ "Test 1: interpolateString [(\"name\", \"Alice\")] \"Hello, ${name}!\"" ~: do
        result <- timeout (3 * 1000000) (evaluate $ HW1.interpolateString [("name", "Alice")] "Hello, ${name}!")
        assertEqual "Expected interpolated string" (Just $ Right "Hello, Alice!") result
    , "Test 2: interpolateString [(\"num1\", \"42\"), (\"num2\", \"7\")] \"My favorite numbers are ${num1} and ${num2}.\"" ~: do
        result <- timeout (3 * 1000000) (evaluate $ HW1.interpolateString [("num1", "42"), ("num2", "7")] "My favorite numbers are ${num1} and ${num2}.")
        assertEqual "Expected interpolated string" (Just $ Right "My favorite numbers are 42 and 7.") result
    , "Test 3: interpolateString [] \"Hello, ${name}!\"" ~: do
        result <- timeout (3 * 1000000) (evaluate $ HW1.interpolateString [] "Hello, ${name}!")
        assertEqual "Expected MissingVar error" (Just $ Left (HW1.MissingVar "name")) result
    , "Test 4: interpolateString [(\"name\", \"Alice\"), (\"age\", \"30\")] \"Hello, ${name}!\"" ~: do
        result <- timeout (3 * 1000000) (evaluate $ HW1.interpolateString [("name", "Alice"), ("age", "30")] "Hello, ${name}!")
        assertEqual "Expected interpolated string" (Just $ Right "Hello, Alice!") result
    , "Test 5: interpolateString [(\"name\", \"Bob\"), (\"amount\", \"100\")] \"Hello, ${name}! Your balance is ${amount}.\"" ~: do
        result <- timeout (3 * 1000000) (evaluate $ HW1.interpolateString [("name", "Bob"), ("amount", "100")] "Hello, ${name}! Your balance is ${amount}.")
        assertEqual "Expected interpolated string" (Just $ Right "Hello, Bob! Your balance is 100.") result
    , "Test 6: interpolateString [(\"NAME\", \"Alice\")] \"Hello, ${name}!\"" ~: do
        result <- timeout (3 * 1000000) (evaluate $ HW1.interpolateString [("NAME", "Alice")] "Hello, ${name}!")
        assertEqual "Expected MissingVar error" (Just $ Left (HW1.MissingVar "name")) result
    , "Test 7: interpolateString [(\"name\", \"Alice\")] \"Hello, ${name}! Your balance is ${amount}.\"" ~: do
        result <- timeout (3 * 1000000) (evaluate $ HW1.interpolateString [("name", "Alice")] "Hello, ${name}! Your balance is ${amount}.")
        assertEqual "Expected MissingVar error" (Just $ Left (HW1.MissingVar "amount")) result
    ]


testRange :: Test
testRange =
  test
    [
        "Test 1: range 5" ~: HW1.range 5 ~?= [0, 1, 2, 3, 4]
      , "Test 2: range -4" ~: do
    		result <- timeout (3 * 1000000) (evaluate $ null (HW1.range (-4)))
    		assertEqual "Expected range -4 to be empty" (Just True) result
      , "Test 3: range 0" ~: do
    		result <- timeout (3 * 1000000) (evaluate $ null (HW1.range 0))
    		assertEqual "Expected range 0 to be empty" (Just True) result
    ]

testEnumerate :: Test
testEnumerate =
  test
  [
      "Test 1: enumerate \"Hello\"" ~: HW1.enumerate "Hello" ~?= [(0, 'H'), (1, 'e'), (2, 'l'), (3, 'l'), (4, 'o')]
    , "Test 2: enumerate \"\"" ~: HW1.enumerate "" ~?= []
  ]

testSplits :: Test
testSplits =
  test
  [
      "Test 1: splits [1, 2, 3]" ~: HW1.splits [1, 2, 3] ~?= [([1, 2, 3],[]), ([1, 2], [3]), ([1], [2, 3]), ([], [1, 2, 3])]
    , "Test 2: splits ([] :: [Int])" ~: HW1.splits ([] :: [Int]) ~?= [([], [])]
    , "Test 3: splits ['H']" ~: HW1.splits ['H'] ~?= [(['H'],[]), ([], ['H'])]
  ]


areListsEqual :: (Ord a) => [a] -> [a] -> Bool
areListsEqual list1 list2 = Set.fromList list1 == Set.fromList list2


testPermutations :: Test
testPermutations =
  test
  [
      "Test 1: permutations \"HEL\"" ~: areListsEqual (HW1.permutations "HEL") [['H','E', 'L'], ['H', 'L', 'E'], ['L', 'H', 'E'], ['L', 'E', 'H'], ['E', 'L', 'H'], ['E', 'H', 'L']] ~?= True
    , "Test 2: permutations [1]" ~: HW1.permutations [1] ~?= [[1]]
    , "Test 3: permutations ([] :: [Int])" ~: HW1.permutations ([] :: [Int]) ~?= [[]]
    , "Test 4: permutations [1,1,2])" ~: areListsEqual (HW1.permutations [1,1,2]) [[1,1,2],[1,2,1],[2,2,1]] ~?= True
  ]


testQueens :: Test
testQueens =
  test
    [ "Test 1: queens 4" ~: areListsEqual (HW1.queens 4) [[2,0,3,1],[1,3,0,2]] ~?= True
    , "Test 2: queens 8" ~: areListsEqual (HW1.queens 8) [[7,3,0,2,5,1,6,4],[7,2,0,5,1,4,6,3],[7,1,4,2,0,6,3,5],[7,1,3,0,6,4,2,5],[6,4,2,0,5,7,1,3],[6,3,1,7,5,0,2,4],[6,3,1,4,7,0,2,5],[6,2,7,1,4,0,5,3],[6,2,0,5,7,4,1,3],[6,1,5,2,0,3,7,4],[6,1,3,0,7,4,2,5],[6,0,2,7,5,3,1,4],[5,7,1,3,0,6,4,2],[5,3,6,0,7,1,4,2],[5,3,6,0,2,4,1,7],[5,3,1,7,4,6,0,2],[5,3,0,4,7,1,6,2],[5,2,6,3,0,7,1,4],[5,2,6,1,7,4,0,3],[5,2,6,1,3,7,0,4],[5,2,4,7,0,3,1,6],[5,2,4,6,0,3,1,7],[5,2,0,7,4,1,3,6],[5,2,0,7,3,1,6,4],[5,2,0,6,4,7,1,3],[5,1,6,0,3,7,4,2],[5,1,6,0,2,4,7,3],[5,0,4,1,7,2,6,3],[4,7,3,0,6,1,5,2],[4,7,3,0,2,5,1,6],[4,6,3,0,2,7,5,1],[4,6,1,5,2,0,7,3],[4,6,1,5,2,0,3,7],[4,6,1,3,7,0,2,5],[4,6,0,3,1,7,5,2],[4,6,0,2,7,5,3,1],[4,2,7,3,6,0,5,1],[4,2,0,6,1,7,5,3],[4,2,0,5,7,1,3,6],[4,1,7,0,3,6,2,5],[4,1,5,0,6,3,7,2],[4,1,3,6,2,7,5,0],[4,1,3,5,7,2,0,6],[4,0,7,5,2,6,1,3],[4,0,7,3,1,6,2,5],[4,0,3,5,7,1,6,2],[3,7,4,2,0,6,1,5],[3,7,0,4,6,1,5,2],[3,7,0,2,5,1,6,4],[3,6,4,2,0,5,7,1],[3,6,4,1,5,0,2,7],[3,6,2,7,1,4,0,5],[3,6,0,7,4,1,5,2],[3,5,7,2,0,6,4,1],[3,5,7,1,6,0,2,4],[3,5,0,4,1,7,2,6],[3,1,7,5,0,2,4,6],[3,1,7,4,6,0,2,5],[3,1,6,4,0,7,5,2],[3,1,6,2,5,7,4,0],[3,1,6,2,5,7,0,4],[3,1,4,7,5,0,2,6],[3,0,4,7,5,2,6,1],[3,0,4,7,1,6,2,5],[2,7,3,6,0,5,1,4],[2,6,1,7,5,3,0,4],[2,6,1,7,4,0,3,5],[2,5,7,1,3,0,6,4],[2,5,7,0,4,6,1,3],[2,5,7,0,3,6,4,1],[2,5,3,1,7,4,6,0],[2,5,3,0,7,4,6,1],[2,5,1,6,4,0,7,3],[2,5,1,6,0,3,7,4],[2,5,1,4,7,0,6,3],[2,4,7,3,0,6,1,5],[2,4,6,0,3,1,7,5],[2,4,1,7,5,3,6,0],[2,4,1,7,0,6,3,5],[2,0,6,4,7,1,3,5],[1,7,5,0,2,4,6,3],[1,6,4,7,0,3,5,2],[1,6,2,5,7,4,0,3],[1,5,7,2,0,3,6,4],[1,5,0,6,3,7,2,4],[1,4,6,3,0,7,5,2],[1,4,6,0,2,7,5,3],[1,3,5,7,2,0,6,4],[0,6,4,7,1,3,5,2],[0,6,3,5,7,1,4,2],[0,5,7,2,6,3,1,4],[0,4,7,5,2,6,1,3]] ~?= True
    , "Test 4: queens 2" ~: areListsEqual (HW1.queens 2) [] ~?= True
    , "Test 5: queens 1" ~: areListsEqual (HW1.queens 1) [[0]] ~?= True
    , "Test 6: queens 5" ~: areListsEqual (HW1.queens 5) [[4,2,0,3,1],[4,1,3,0,2],[3,1,4,2,0],[3,0,2,4,1],[2,4,1,3,0],[2,0,3,1,4],[1,4,2,0,3],[1,3,0,2,4],[0,3,1,4,2],[0,2,4,1,3]] ~?= True
    ]

 
allTests :: Test
allTests =
  test
    [ "testFromMaybe" ~: testFromMaybe
    , "testMaybe" ~: testMaybe
    , "testCatMaybes" ~: testCatMaybes
    , "testMapMaybe" ~: testMapMaybe
    , "testEither" ~: testEither
    , "testMapLeft" ~: testMapLeft
    , "testCatEithers" ~: testCatEithers
    , "testMapEither" ~: testMapEither
    , "testPartitionEithers" ~: testPartitionEithers
    , "testSnoc" ~: testSnoc
    , "testZipWith" ~: testZipWith
    , "testZip" ~: testZip
    , "testUnzip" ~: testUnzip
    , "testSplitOn" ~: testSplitOn
    , "testParseTemplate" ~: testParseTemplate
    , "testAssignTemplate" ~: testAssignTemplate
    , "testInterpolateString" ~: testInterpolateString
    , "testRange" ~: testRange
    , "testEnumerate" ~: testEnumerate
    , "testSplits" ~: testSplits
    , "testPermutations" ~: testPermutations
    , "testQueens" ~: testQueens
    ] 

-- Run the tests and output the results to a file
main :: IO ()
main = do
  let outputFile = "test_results.txt"
  (counts, failures) <- runTestText (putTextToHandle stdout False) allTests
  putStrLn $ ""
