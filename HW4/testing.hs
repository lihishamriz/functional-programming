{-# LANGUAGE StandaloneDeriving #-}
import Test.HUnit
import System.IO
import Control.Exception (evaluate)
import Data.Char ( toUpper, toLower )
import Data.List (sort)
import Data.Ord
import Data.Text
import Data.Monoid
import qualified HW4
import Test.HUnit (assertEqual)
import System.Timeout

import Deque as DQ
import PersistentArray as PA


testWithTimeout :: (Eq a, Show a) => Int -> a -> a -> Assertion
testWithTimeout time expected action = do
  result <- timeout time (evaluate $ action)
  case result of
    Just value -> assertEqual "" expected value
    Nothing -> assertEqual "Timeout expired" False True


testTimeExpired time expected action = do
  result <- timeout time (evaluate $ action)
  case result of
    Just _ -> assertFailure "Expected timeout, but action completed"
    Nothing -> assertEqual "" expected expected


testFold :: Test
testFold = test
  [ "Test 1: fold $ map Sum [1,2,3,4,5]" ~: testWithTimeout (1 * 1000000) (Data.Monoid.Sum 15) (HW4.fold $ Prelude.map Data.Monoid.Sum [1,2,3,4,5])
  , "Test 2: fold $ map Product [1,2,3,4,5]" ~: testWithTimeout (1 * 1000000) (Data.Monoid.Product 120) (HW4.fold $ Prelude.map Data.Monoid.Product [1,2,3,4,5])
  , "Test 3: fold $ map Any [False, True, False]" ~: testWithTimeout (1 * 1000000) (Data.Monoid.Any True) (HW4.fold $ Prelude.map Data.Monoid.Any [False, True, False])
  , "Test 4: fold $ map First [Just 1, Just 2, Nothing]" ~: testWithTimeout (1 * 1000000) (Data.Monoid.First (Just 1)) (HW4.fold $ Prelude.map Data.Monoid.First [Just 1, Just 2, Nothing])
  , "Test 5: fold $ map Last [Just 1, Just 2, Nothing]" ~: testWithTimeout (1 * 1000000) (Data.Monoid.Last (Just "sdf")) (HW4.fold $ Prelude.map Data.Monoid.Last [Just "fdg", Just "sdf", Nothing])
  , "Test 6: fold $ map Any (repeat True)" ~: testWithTimeout (1 * 1000000) (Data.Monoid.Any True) (HW4.fold $ Prelude.map Data.Monoid.Any (cycle [False, False, False, True]))
  ]


testToList :: Test
testToList = test
  [ "Test 1: toList $ Just 4" ~: testWithTimeout (1 * 1000000) [4] (HW4.toList $ Just 4)
  , "Test 2: toList $ Nothing" ~: testWithTimeout (1 * 1000000) ([] :: [Int]) (HW4.toList Nothing)
  , "Test 3: toList [1,2,3,4,5]" ~: testWithTimeout (1 * 1000000) [1,2,3,4,5] (HW4.toList [1,2,3,4,5])
  , "Test 4: toList (Right 4)" ~: testWithTimeout (1 * 1000000) [4] (HW4.toList $ Right 4)
  , "Test 5: toList (Just \"Hello\")" ~: testWithTimeout (1 * 1000000) ["Hello"] (HW4.toList $ Just "Hello")
  , "Test 6: toList [1..]" ~: testWithTimeout (1 * 1000000) [1,2,3,4,5] (Prelude.take 5 $ HW4.toList [1..])
  ]

testElem :: Test
testElem = test
  [ "Test 1: elem 3 [1,2,3,4,5]" ~: testWithTimeout (1 * 1000000) True (HW4.elem 3 [1,2,3,4,5])
  , "Test 2: elem 10 [1,2,3,4,5]" ~: testWithTimeout (1 * 1000000) False (HW4.elem 10 [1,2,3,4,5])
  , "Test 3: elem 'a' \"Hello\"" ~: testWithTimeout (1 * 1000000) False (HW4.elem 'a' "Hello")
  , "Test 4: elem 'l' \"Hello\"" ~: testWithTimeout (1 * 1000000) True (HW4.elem 'l' "Hello")
  , "Test 5: elem 1 Nothing" ~: testWithTimeout (1 * 1000000) False (HW4.elem 1 Nothing)
  , "Test 6: elem 1 (Just 1)" ~: testWithTimeout (1 * 1000000) True (HW4.elem 1 (Just 1))
  , "Test 7: elem 6 (cycle [1,2,3,4,5])" ~: testTimeExpired (1 * 1000) False (HW4.elem 6 (cycle [1,2,3,4,5]))
  , "Test 8: elem 999 [1..]" ~: testWithTimeout (1 * 1000000) True (HW4.elem 999 [1..])
  ]

testFind :: Test
testFind = test
  [ "Test 1: find even [1, 3, 4, 6, 8]" ~: testWithTimeout (1 * 1000000) (Just 4) (HW4.find even [1, 3, 4, 6, 8])
  , "Test 2: find even [1, 3, 5, 7, 9]" ~: testWithTimeout (1 * 1000000) Nothing (HW4.find even [1, 3, 5, 7, 9])
  , "Test 3: find even [2, 4, 6, 8, 10]" ~: testWithTimeout (1 * 1000000) (Just 2) (HW4.find even [2, 4, 6, 8, 10])
  , "Test 4: find even [2, 4, 6, 2, 8]" ~: testWithTimeout (1 * 1000000) (Just 2) (HW4.find even [2, 4, 6, 2, 8])
  , "Test 5: find even [1, 3, 5, 2, 4, 6]" ~: testWithTimeout (1 * 1000000) (Just 2) (HW4.find even [1, 3, 5, 2, 4, 6])
  , "Test 6: find even (cycle [1, 2, 3, 4, 5])" ~: testTimeExpired (1 * 1000) True (HW4.find even (cycle [1, 3, 5, 7]))
  , "Test 7: find even [1 ..]" ~: testWithTimeout (1 * 1000000) (Just 2) (HW4.find even [1 ..])
  ]


testLength :: Test
testLength = test
  [ "Test 1: length [1,2,3,4,5]" ~: testWithTimeout (1 * 1000000) 5 (HW4.length [1,2,3,4,5])
  , "Test 2: length \"Hello\"" ~: testWithTimeout (1 * 1000000) 5 (HW4.length "Hello")
  , "Test 3: length (Just 4)" ~: testWithTimeout (1 * 1000000) 1 (HW4.length (Just 4))
  , "Test 4: length Nothing" ~: testWithTimeout (1 * 1000000) 0 (HW4.length Nothing)
  , "Test 5: length (Right 4)" ~: testWithTimeout (1 * 1000000) 1 (HW4.length (Right 4))
  , "Test 6: length (Left \"Error\")" ~: testWithTimeout (1 * 1000000) 0 (HW4.length (Left "Error"))
  ]

testNull :: Test
testNull = test 
  [ "Test 1: null [1,2,3,4,5]" ~: testWithTimeout (1 * 1000000) False (HW4.null [1,2,3,4,5])
  , "Test 2: null \"\"" ~: testWithTimeout (1 * 1000000) True (HW4.null "")
  , "Test 3: null (Just 4)" ~: testWithTimeout (1 * 1000000) False (HW4.null (Just 4))
  , "Test 4: null Nothing" ~: testWithTimeout (1 * 1000000) True (HW4.null Nothing)
  , "Test 5: null [1..]" ~: testWithTimeout (1 * 1000000) False (HW4.null [1..])
  ]

testMaximum :: Test
testMaximum = test
  [ "Test 1: maximum [1,2,3,4,5]" ~: testWithTimeout (1 * 1000000) (Just 5) (HW4.maximum ([1,2,3,4,5] :: [Int]))
  , "Test 2: maximum []" ~: testWithTimeout (1 * 1000000) (Nothing :: Maybe Int) (HW4.maximum ([] :: [Int]))
  ]

testMaxBy :: Test
testMaxBy = test
  [ "Test 1: maxBy Prelude.length [\"Hello\", \"world\", \"!\"]" ~: testWithTimeout (1 * 1000000) (Just "Helloo") (HW4.maxBy Prelude.length ["Helloo", "world", "!"])
  , "Test 2: maxBy Prelude.length []" ~: testWithTimeout (1 * 1000000) (Nothing :: Maybe String) (HW4.maxBy Prelude.length ([] :: [String]))
  , "Test 3: maxBy id (Just 4)" ~: testWithTimeout (1 * 1000000) (Just 4) (HW4.maxBy id (Just 4))
  , "Test 4: maxBy id Nothing" ~: testWithTimeout (1 * 1000000) (Nothing :: Maybe Int) (HW4.maxBy id (Nothing :: Maybe Int))
  , "Test 5: maxBy negate [1, 2, 3, 4, 5]" ~: testWithTimeout (1 * 1000000) (Just 1) (HW4.maxBy negate [1, 2, 3, 4, 5])
  , "Test 6: maxBy (^2) [-2, -1, 0, 1, 2]" ~: testWithTimeout (1 * 1000000) (Just (-2)) (HW4.maxBy (^2) [-2, -1, 0, 1])
  ]

testMinimum :: Test
testMinimum = test 
  [ "Test 1: minimum [1,2,3,4,5]" ~: Just 1 ~=? (HW4.minimum [1,2,3,4,5])
  , "Test 2: minimum []" ~: Nothing ~=? (HW4.minimum ([] :: [Int]))
  ]

testMinBy :: Test
testMinBy = test 
  [ "Test 1: minBy Prelude.length [\"Hello\", \"world\", \"!\"]" ~: testWithTimeout (1 * 1000000) (Just "!") (HW4.minBy Prelude.length ["Hello", "world", "!"])
  , "Test 2: minBy Prelude.length []" ~: testWithTimeout (1 * 1000000) (Nothing :: Maybe String) (HW4.minBy Prelude.length ([] :: [String]))
  , "Test 3: minBy id (Just 4)" ~: testWithTimeout (1 * 1000000) (Just 4) (HW4.minBy id (Just 4))
  , "Test 4: minBy id Nothing" ~: testWithTimeout (1 * 1000000) (Nothing :: Maybe Int) (HW4.minBy id (Nothing :: Maybe Int))
  , "Test 5: minBy negate [1, 2, 3, 4, 5]" ~: testWithTimeout (1 * 1000000) (Just 5) (HW4.minBy negate [1, 2, 3, 4, 5])
  , "Test 6: minBy (^2) [-2, -1, 0, 1, 2]" ~: testWithTimeout (1 * 1000000) (Just (0)) (HW4.minBy (^2) [-2, -1, 0, 1, 2])
  ]

testSum :: Test
testSum = test 
  [ "Test 1: sum [1,2,3,4,5]" ~: 15 ~=? (HW4.sum [1,2,3,4,5])
  , "Test 2: sum []" ~: 0 ~=? (HW4.sum ([] :: [Int]))
  , "Test 3: sum (Just 4)" ~: 4 ~=? (HW4.sum (Just 4))
  , "Test 4: sum Nothing" ~: 0 ~=? (HW4.sum Nothing)
  ]

testProduct :: Test
testProduct = test 
  [ "Test 1: product [1,2,3,4,5]" ~: 120 ~=? (HW4.product [1,2,3,4,5])
  , "Test 2: product []" ~: 1 ~=? (HW4.product ([] :: [Int]))
  , "Test 3: product (Just 4)" ~: 4 ~=? (HW4.product (Just 4))
  , "Test 4: product Nothing" ~: 1 ~=? (HW4.product Nothing)
  ]

testFmapToFst :: Test
testFmapToFst = test 
  [ "Test 1: fmapToFst (+1) [1,2,3,4,5]" ~: testWithTimeout (1 * 1000000) [(2,1),(3,2),(4,3),(5,4),(6,5)] (HW4.fmapToFst (+1) [1,2,3,4,5])
  , "Test 2: fmapToFst show (Just 4)" ~: testWithTimeout (1 * 1000000) (Just ("4",4)) (HW4.fmapToFst show (Just 4))
  , "Test 3: fmapToFst Prelude.length (Just \"Hello\")" ~: testWithTimeout (1 * 1000000) (Just (5,"Hello")) (HW4.fmapToFst Prelude.length (Just "Hello"))
  ]


testFmapToSnd :: Test
testFmapToSnd = test 
  [ "Test 1: fmapToSnd (+1) [1,2,3,4,5]" ~: testWithTimeout (1 * 1000000) [(1,2),(2,3),(3,4),(4,5),(5,6)] (HW4.fmapToSnd (+1) [1,2,3,4,5])
  , "Test 2: fmapToSnd show (Just 4)" ~: testWithTimeout (1 * 1000000) (Just (4,"4")) (HW4.fmapToSnd show (Just 4))
  , "Test 3: fmapToSnd Prelude.length (Just \"Hello\")" ~: testWithTimeout (1 * 1000000) (Just ("Hello",5)) (HW4.fmapToSnd Prelude.length (Just "Hello"))
  ]

testStrengthenR :: Test
testStrengthenR = test
  [ "Test 1: strengthenR 1 (Just 2)" ~: testWithTimeout (1 * 1000000) (Just (2, 1)) (HW4.strengthenR 1 (Just 2))
  , "Test 2: strengthenR 'a' (Just 2)" ~: testWithTimeout (1 * 1000000) (Just (2, 'a')) (HW4.strengthenR 'a' (Just 2))
  , "Test 3: strengthenR 1 Nothing" ~: testWithTimeout (1 * 1000000) (Nothing :: Maybe (Int, Int)) (HW4.strengthenR 1 (Nothing :: Maybe Int))
  , "Test 4: strengthenR 1 [1, 2, 3]" ~: testWithTimeout (1 * 1000000) [(1, 1), (2, 1), (3, 1)] (HW4.strengthenR 1 [1, 2, 3])
  ]

testStrengthenL :: Test
testStrengthenL = test
  [ "Test 1: strengthenL 1 (Just 2)" ~: testWithTimeout (1 * 1000000) (Just (1, 2)) (HW4.strengthenL 1 (Just 2))
  , "Test 2: strengthenL 'a' (Just 2)" ~: testWithTimeout (1 * 1000000) (Just ('a', 2)) (HW4.strengthenL 'a' (Just 2))
  , "Test 3: strengthenL 1 Nothing" ~: testWithTimeout (1 * 1000000) (Nothing :: Maybe (Int, Int)) (HW4.strengthenL 1 (Nothing :: Maybe Int))
  , "Test 4: strengthenL 1 [1, 2, 3]" ~: testWithTimeout (1 * 1000000) [(1, 1), (1, 2), (1, 3)] (HW4.strengthenL 1 [1, 2, 3])
  ]

unzipTests :: Test
unzipTests = test
  [ "Test 1: unzip (Just (1, 2))" ~: testWithTimeout (1 * 1000000) ((Just 1, Just 2)) (HW4.unzip (Just (1, 2)))
  , "Test 2: unzip [ (1, 'a'), (2, 'b')]" ~: testWithTimeout (1 * 1000000) (([1, 2], ['a', 'b'])) (HW4.unzip [(1, 'a'), (2, 'b')])
  , "Test 3: unzip Nothing" ~: testWithTimeout (1 * 1000000) ((Nothing, Nothing) :: (Maybe Int, Maybe Int)) (HW4.unzip (Nothing :: Maybe (Int, Int)))
  ]

coUnzipTests :: Test
coUnzipTests = test
  [ "Test 1: coUnzip (Right (Just 1))" ~: testWithTimeout (1 * 1000000) (Just (Right 1) :: Maybe (Either Int Int)) (HW4.coUnzip (Right (Just 1)))
  , "Test 2: coUnzip (Left (Just 'a'))" ~: testWithTimeout (1 * 1000000) (Just (Left 'a') :: Maybe (Either Char Char)) (HW4.coUnzip (Left (Just 'a')))
  , "Test 3: coUnzip (Right Nothing)" ~: testWithTimeout (1 * 1000000) (Nothing :: Maybe (Either Int Char)) (HW4.coUnzip (Right (Nothing :: Maybe a)))
  , "Test 4: coUnzip (Right [1, 2, 3])" ~: testWithTimeout (1 * 1000000) ([Right 1, Right 2, Right 3] :: [Either Int Int]) (HW4.coUnzip (Right [1, 2, 3]))
  , "Test 5: coUnzip (Left ['a', 'b', 'c'])" ~: testWithTimeout (1 * 1000000) ([Left 'a', Left 'b', Left 'c'] :: [Either Char Char]) (HW4.coUnzip (Left ['a', 'b', 'c']))
  ]


fromListTests :: Test
fromListTests = test
  [ "Test 1: fromList [1, 2, 3]" ~: testWithTimeout (1 * 1000000) [1, 2, 3] (HW4.fromList [1, 2, 3])
  , "Test 2: fromList ['a', 'b', 'c']" ~: testWithTimeout (1 * 1000000) ['a', 'b', 'c'] (HW4.fromList ['a', 'b', 'c'])
  , "Test 3: fromList (take 1000000 [1..])" ~: testWithTimeout (1 * 1000000) (Prelude.take 1000000 [1..]) (HW4.fromList (Prelude.take 1000000 [1..]))
  ]


unfoldrTests :: Test
unfoldrTests = test
  [ "Test 1: unfoldr (> 5)" ~: testWithTimeout (1 * 1000000) [] (HW4.unfoldr (\x -> if x > 5 then Just (x, x + 1) else Nothing) 1 :: [Int])
  , "Test 2: unfoldr (<= 5)" ~: testWithTimeout (1 * 1000000) [1, 2, 3, 4, 5] (HW4.unfoldr (\x -> if x <= 5 then Just (x, x + 1) else Nothing) 1 :: [Int])
  , "Test 3: unfoldr (always Just)" ~: testWithTimeout (1 * 1000000) [1, 2, 3, 4, 5] (Prelude.take 5 $ (HW4.unfoldr (\x -> Just (x, x + 1)) 1 :: [Int]))
  , "Test 4: unfoldr (alternate sign)" ~: testWithTimeout (1 * 1000000) [1, -2, 3, -4, 5] (Prelude.take 5 $ (HW4.unfoldr (\x -> Just (x, if x > 0 then -(x + 1) else -(x - 1))) 1 :: [Int]))
  , "Test 5: unfoldr (Fibonacci sequence)" ~: testWithTimeout (1 * 1000000) [0, 1, 1, 2, 3, 5, 8] (Prelude.take 7 $ (HW4.unfoldr (\(a, b) -> Just (a, (b, a + b))) (0, 1) :: [Int]))
  , "Test 6: unfoldr (prime numbers)" ~: testWithTimeout (1 * 1000000) [2, 3, 5, 7, 11, 13] (Prelude.take 6 $ (HW4.unfoldr (\n -> Just (n, nextPrime (n))) 2 :: [Int]))
  ]

nextPrime :: Int -> Int
nextPrime n
  | isPrime (n + 1) = n + 1
  | otherwise = nextPrime (n + 1)

isPrime :: Int -> Bool
isPrime n
  | n <= 1 = False
  | n <= 3 = True
  | n `mod` 2 == 0 || n `mod` 3 == 0 = False
  | otherwise = checkPrime n 5

checkPrime :: Int -> Int -> Bool
checkPrime n i
  | i * i > n = True
  | n `mod` i == 0 || n `mod` (i + 2) == 0 = False
  | otherwise = checkPrime n (i + 6)



unfoldableTests :: Test
unfoldableTests = test
  [ "Test 1: Unfoldable PersistentArray - fromList" ~: testWithTimeout (1 * 1000000) expected1 (toList $ (HW4.fromList input1 :: PA.PersistentArray Int))
  , "Test 2: Unfoldable PersistentArray - fromList with empty list" ~: testWithTimeout (1 * 1000000) expected2 (toList $ (HW4.fromList input2 :: PA.PersistentArray Int))
  , "Test 3: Unfoldable PersistentArray - fromList with duplicate values" ~: testWithTimeout (1 * 1000000) expected3 (toList $ (HW4.fromList input3 :: PA.PersistentArray Int))
  , "Test 4: Unfoldable Deque - fromList" ~: testWithTimeout (1 * 1000000) expected1 (toList $ (HW4.fromList input1 :: DQ.Deque Int))
  , "Test 5: Unfoldable Deque - fromList with empty list" ~: testWithTimeout (1 * 1000000) expected2 (toList $ (HW4.fromList input2 :: DQ.Deque Int))
  , "Test 6: Unfoldable Deque - fromList with duplicate values" ~: testWithTimeout (1 * 1000000) expected3 (toList $ (HW4.fromList input3 :: DQ.Deque Int))
  , "Test 7: Unfoldable List - fromList" ~: testWithTimeout (1 * 1000000) expected1 (toList $ (HW4.fromList input1 :: [Int]))
  , "Test 8: Unfoldable List - fromList with empty list" ~: testWithTimeout (1 * 1000000) expected2 (toList $ (HW4.fromList input2 :: [Int]))
  , "Test 9: Unfoldable List - fromList with duplicate values" ~: testWithTimeout (1 * 1000000) expected3 (toList $ (HW4.fromList input3 :: [Int]))
  ]
  where
    input1 = [1, 2, 3, 4, 5]
    expected1 = [1, 2, 3, 4, 5]
    input2 = []
    expected2 = []
    input3 = [1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5]
    expected3 = [1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5]
    toList :: Foldable t => t a -> [a]
    toList = foldMap (: [])


foldableTests :: Test
foldableTests = test
  [ "Test 1: Foldable Deque - sum" ~: testWithTimeout (1 * 1000000) 15 (HW4.sum deque)
  , "Test 2: Foldable Deque - product" ~: testWithTimeout (1 * 1000000) 120 (HW4.product deque)
  , "Test 3: Foldable Deque - length" ~: testWithTimeout (1 * 1000000) 5 (HW4.length deque)
  , "Test 4: Foldable PersistentArray - sum" ~: testWithTimeout (1 * 1000000) 15 (HW4.sum array)
  , "Test 5: Foldable PersistentArray - product" ~: testWithTimeout (1 * 1000000) 120 (HW4.product array)
  , "Test 6: Foldable PersistentArray - length" ~: testWithTimeout (1 * 1000000) 5 (HW4.length array)

  ]
  where
    deque :: DQ.Deque Int
    deque = Prelude.foldr DQ.pushr DQ.empty [1, 2, 3, 4, 5]
    array :: PA.PersistentArray Int
    array = Prelude.foldr PA.pushr PA.empty [1, 2, 3, 4, 5]



functorTests :: Test
functorTests = test
  [ "Test 1: Functor Deque - fmap (+1)" ~: testWithTimeout (1 * 1000000) [2, 3, 4, 5, 6] (toList $ fmap (+1) deque)
  , "Test 2: Functor Deque - fmap (*2)" ~: testWithTimeout (1 * 1000000) [2, 4, 6, 8, 10] (toList $ fmap (*2) deque)
  , "Test 3: Functor Deque - fmap show" ~: testWithTimeout (1 * 1000000) ["1", "2", "3", "4", "5"] (toList $ fmap show deque)
  , "Test 4: Functor PA - fmap (+1)" ~: testWithTimeout (1 * 1000000) [2, 3, 4, 5, 6] (toList $ fmap (+1) array)
  , "Test 5: Functor PA - fmap (*2)" ~: testWithTimeout (1 * 1000000) [2, 4, 6, 8, 10] (toList $ fmap (*2) array)
  , "Test 6: Functor PA - fmap show" ~: testWithTimeout (1 * 1000000) ["1", "2", "3", "4", "5"] (toList $ fmap show array)
  ]
  where
    deque :: DQ.Deque Int
    deque = Prelude.foldr DQ.pushl DQ.empty [1, 2, 3, 4, 5]
    array :: PA.PersistentArray Int
    array = Prelude.foldr PA.pushr PA.empty [5, 4, 3, 2, 1]
    toList :: Foldable t => t a -> [a]
    toList = foldMap (: [])


semigroupTests :: Test
semigroupTests = test
  [ "Test 1: Semigroup Deque - append" ~: testWithTimeout (1 * 1000000) [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] (toList $ deque1 <> deque2)
  , "Test 2: Semigroup PA - append" ~: testWithTimeout (1 * 1000000) [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] (toList $ array1 <> array2)
  , "Test 3: Semigroup Deque - append (same values)" ~: testWithTimeout (1 * 1000000) [1, 2, 3, 4, 5, 1, 2, 3, 4, 5] (toList $ deque1 <> deque1)
  , "Test 4: Semigroup PA - append (same values)" ~: testWithTimeout (1 * 1000000) [1, 2, 3, 4, 5, 1, 2, 3, 4, 5] (toList $ array1 <> array1)
  ]
  where
    deque1 :: DQ.Deque Int
    deque1 = Prelude.foldr DQ.pushl DQ.empty [1, 2, 3, 4, 5]
    deque2 :: DQ.Deque Int
    deque2 = Prelude.foldr DQ.pushl DQ.empty [6, 7, 8, 9, 10]
    array1 :: PA.PersistentArray Int
    array1 = Prelude.foldr PA.pushr PA.empty [5, 4, 3, 2, 1]
    array2 :: PA.PersistentArray Int
    array2 = Prelude.foldr PA.pushr PA.empty [10, 9, 8, 7, 6]
    toList :: Foldable t => t a -> [a]
    toList = foldMap (: [])



monoidTests :: Test
monoidTests = test
  [ "Test 1: Monoid Deque - mempty" ~: testWithTimeout (1 * 1000000) [] (toList (mempty :: DQ.Deque Int))
  , "Test 2: Monoid PA - mempty" ~: testWithTimeout (1 * 1000000) [] (toList (mempty :: PA.PersistentArray Int))
  ]
  where
    toList :: Foldable t => t a -> [a]
    toList = foldMap (: [])


semigroupZipListTests :: Test
semigroupZipListTests = test
  [ "Test 1: Semigroup ZipList - element-wise addition" ~: testWithTimeout (1 * 1000000) [5, 7] (toList $ zipList1 <> zipList2)
  , "Test 2: Semigroup ZipList - element-wise multiplication" ~: testWithTimeout (1 * 1000000) [0, 2, 6, 12, 20] (Prelude.take 5 $ toList $ zipList3 <> zipList4)
  , "Test 3: Semigroup ZipList - empty list" ~: testWithTimeout (1 * 1000000) [] (toList $ zipList1 <> zipList5)
  , "Test 4: Semigroup ZipList - different list lengths" ~: testWithTimeout (1 * 1000000) [Sum {getSum = 2},Sum {getSum = 4}] (Prelude.take 4 $ toList $ zipList1 <> zipList6)
  , "Test 5: Semigroup ZipList - infinite lists" ~: testWithTimeout (1 * 1000000) [2, 6, 10, 14, 18] (Prelude.take 5 $ toList $ zipList7 <> zipList8)
  ]
  where
    zipList1 :: HW4.ZipList (Sum Int)
    zipList1 = HW4.ZipList (Prelude.map Sum [1, 2])

    zipList2 :: HW4.ZipList (Sum Int)
    zipList2 = HW4.ZipList (Prelude.map Sum [4, 5])

    zipList3 :: HW4.ZipList (Product Int)
    zipList3 = HW4.ZipList (Prelude.map Product [1 ..])

    zipList4 :: HW4.ZipList (Product Int)
    zipList4 = HW4.ZipList (Prelude.map Product [0 ..])

    zipList5 :: HW4.ZipList (Sum Int)
    zipList5 = HW4.ZipList []

    zipList6 :: HW4.ZipList (Sum Int)
    zipList6 = HW4.ZipList (Prelude.map Sum [1, 2, 3, 4])

    zipList7 :: HW4.ZipList (Sum Int)
    zipList7 = HW4.ZipList (Prelude.map Sum [1, 2 ..])

    zipList8 :: HW4.ZipList (Sum Int)
    zipList8 = HW4.ZipList (Prelude.map Sum [1, 4 ..])

    toList :: HW4.ZipList a -> [a]
    toList (HW4.ZipList xs) = xs


monoidZipListTests :: Test
monoidZipListTests = test
  [ "Test 1: Monoid ZipList - left identity" ~: testWithTimeout (1 * 1000000) [[Sum {getSum = 1}], [Sum {getSum = 2}], [Sum {getSum = 3}]] (Prelude.take 3 $ toList $ mempty <> zipList1)
  , "Test 2: Monoid ZipList - right identity" ~: testWithTimeout (1 * 1000000) [[Sum {getSum = 1}], [Sum {getSum = 2}], [Sum {getSum = 3}]] (Prelude.take 3 $ toList $ zipList1 <> mempty)
  , "Test 3: Monoid ZipList - Infinite list" ~: testWithTimeout (1 * 1000000) [[Sum {getSum = 4}], [Sum {getSum = 4}], [Sum {getSum = 4}]] (Prelude.take 3 $ toList $ infiniteZipList <> mempty)
  ]
  where
    zipList1 :: HW4.ZipList [Sum Int]
    zipList1 = HW4.ZipList [[Sum 1], [Sum 2], [Sum 3]]

    infiniteZipList :: HW4.ZipList [Sum Int]
    infiniteZipList = HW4.ZipList (repeat [Sum 4])

    toList :: HW4.ZipList [a] -> [[a]]
    toList (HW4.ZipList xs) = xs


allTests :: Test
allTests =
  test
    [ 
      "testFold" ~: testFold
      ,"testToList" ~: testToList
      ,"testElem" ~: testElem
      ,"testFind" ~: testFind
      ,"testLength" ~: testLength
      ,"testNull" ~: testNull
      ,"testMaximum" ~: testMaximum
      ,"testMaxBy" ~: testMaxBy
      ,"testMinimum" ~: testMinimum
      ,"testMinBy" ~: testMinBy
      ,"testSum" ~: testSum
      ,"testProduct" ~: testProduct
      ,"testFmapToFst" ~: testFmapToFst
      ,"testFmapToSnd" ~: testFmapToSnd
      ,"testStrengthenR" ~: testStrengthenR
      ,"testStrengthenL" ~: testStrengthenL
      ,"unzipTests" ~: unzipTests
      ,"coUnzipTests" ~: coUnzipTests
      ,"fromListTests" ~: fromListTests
      ,"unfoldrTests" ~: unfoldrTests
      ,"unfoldableTests" ~: unfoldableTests
      ,"foldableTests" ~: foldableTests
      ,"functorTests" ~: functorTests
      ,"semigroupTests" ~: semigroupTests
      ,"monoidTests" ~: monoidTests
      ,"semigroupZipListTests" ~: semigroupZipListTests
      ,"monoidZipListTests" ~: monoidZipListTests
    ] 

-- Run the tests and output the results to a file
main :: IO ()
main = do
  let outputFile = "test_results.txt"
  (counts, failures) <- runTestText (putTextToHandle stdout False) allTests
  putStrLn $ ""
