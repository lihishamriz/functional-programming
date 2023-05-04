-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW1.hs should successfully compile.

-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}


module HW1 where

import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Int, Maybe (..), Num (..), Ord (..), Show (..), String, all, and, any, concat, concatMap, const, curry, elem, error, filter, flip, foldl, foldr, fst, id, length, lookup, map, not, notElem, null, or, product, reverse, snd, sum, uncurry, undefined, (!!), ($), (&&), (++), (.), (||))
import Distribution.Compat.Lens (_1)
import Distribution.Simple.Utils (xargs)

-- Section 1: Utility functions
-- Basic Maybes
fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just a) = a

maybe :: b -> (a -> b) -> Maybe a -> b
maybe b _ Nothing = b
maybe _ f (Just a) = f a

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr go []
    where
        go x xs = case x of
            Nothing -> xs
            Just y  -> y : xs

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f xs = catMaybes (map f xs)

-- Basic Eithers
either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left a) = f a
either _ g (Right b) = g b

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left $ f a
mapLeft _ (Right b) = Right b

catEithers :: [Either a b] -> Either a [b]
catEithers = foldr go (Right [])
    where
        go (Left a) _ = Left a
        go (Right x) (Right xs) = Right (x : xs)
        go (Right _) (Left a) = Left a  

mapEither :: (a -> Either b c) -> [a] -> Either b [c]
mapEither f xs = catEithers (map f xs)

concatEitherMap :: (b -> Either a c) -> Either a b -> Either a c
concatEitherMap _ (Left a) = Left a
concatEitherMap f (Right b) = f b

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr go ([],[])
    where
        go x (xs, ys) = case x of
            Left a  -> (a : xs, ys)
            Right b -> (xs, b : ys)

{-

>>> fromMaybe 2 (Just 3)
3

>>> maybe 1 length (Just "foo")
3

>>> catMaybes [Just 2, Nothing, Just 3]
[2,3]

>>> mapMaybe (\x -> if x > 0 then Just $ x * 10 else Nothing ) [1, -1, 10]
[10,100]

>>> either length (*10) $ Left "foo"
3

>>> either length (*10) $ Right 10
100

>>> mapLeft (++ "bar") ( Left "foo")
Left "foobar"

>>> mapLeft (++ " bar ") ( Right 10)
Right 10

>>> catEithers [ Right 10 , Right 20]
Right [10,20]

>>> catEithers [ Right 10 , Left "foo", Right 20 , Left " bar "]
Left "foo"

>>> mapEither (\x -> if x > 0 then Right $ x * 10 else Left $ x + 5) [1, 2 , 3]
Right [10,20,30]

>>> mapEither (\x -> if x > 0 then Right $ x * 10 else Left $ x + 5) [1, -1, 2, -2]
Left 4

>>> concatEitherMap ( Right . (* 10) ) ( Left 5)
Left 5

>>> partitionEithers [ Right "foo ", Left 42 , Right " bar ", Left 54]
([42,54],["foo "," bar "])

-}


-- Section 2: Lists and zips
-- Fun with lists and zips
-- snoc is the opposite of cons, i.e., append to list.
snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

-- If one list is shorter than the other, take the shorter one, e.g.,
-- zipWith (+) [1, 2] [3, 4, 5] returns [4, 6]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f as bs = foldr go [] (toZip as bs)
    where
        toZip [] _ = []
        toZip _ [] = []
        toZip (x : xs) (y : ys) = (x ,y) : toZip xs ys

        go (x ,y) zs = f x y : zs 
-- zipWith f = foldr go (const [])
--     where
--         go x zs (y : ys) = f x y : zs ys
--         go _ _ [] = []

-- If one list is shorter than the other, take the shorter one, e.g.,
-- zip [1, 2] [3, 4, 5] returns [(1, 3), (2, 4)]
-- Could you implement this using zipWith and point-free style?
unzip :: [(a, b)] -> ([a], [b])
unzip = foldr go ([], [])
    where
        go (x, y) (xs, ys) = (x:xs, y:ys)

zip :: [a] -> [b] -> [(a, b)]
zip = zipWith (,)

{-

>>> snoc [1,2,3] 4
[1,2,3,4]

>>> zipWith (+) [1 , 2 , 3] [4 , 5 , 6]
[5,7,9]

>>> zipWith (+) [1 , 2] [4 , 5 , 6]
[5,7]

>>> unzip [(1 , 2) , (3 , 4)]
([1,3],[2,4])

>>> zip [1, 2] [3, 4, 5]
[(1,3),(2,4)]

-}


-- Section 3: String interpolation
-- Parsing template strings, e.g., "Hello ${name}!". See the PDF for more information.
splitOn :: Char -> String -> Maybe (String, String)
splitOn c s = go c s []
    where
        go _ [] _ = Nothing
        go c' (x : s') xs = if x == c' then Just (xs, s') else go c s' (snoc xs x)

type Variable = String
data ParsedString = PlainString String | Variable String deriving Show
parseTemplate :: String -> Maybe [ParsedString]
parseTemplate [] = Nothing
parseTemplate s = go s []
    where
        go [] _ = Nothing
        go xs res = case splitOn '$' xs of
            Nothing -> Just (snoc res $ PlainString xs)
            Just(a, b) -> go' b (snoc res $ PlainString a)
                where
                    go' ('{' : ys) res' = case splitOn '}' ys of
                        Nothing -> Nothing
                        Just(c, d) -> go d (snoc res' $ Variable c)
                    go' _ _ = Nothing

-- type VariableName = String
-- type VariableValue = String
-- type MissingVariable = String
-- assignTemplate :: [(VariableName, VariableValue)] -> [ParsedString] -> Either MissingVariable String


{-

>>> splitOn 'x' "foobar"
Nothing

>>> splitOn 'x' "fooxbar"
Just ("foo","bar")

>>> splitOn 'x' "foox"
Just ("foo","")

>>> splitOn 'x' "fooxfooxfoo"
Just ("foo","fooxfoo")

>>> parseTemplate "Hello${world}!"
Just [PlainString "Hello",Variable "world",PlainString "!"]

>>> parseTemplate "Hello${!"
Nothing

>>> parseTemplate "Hello$!"
Nothing

-}

{-

data Error = MissingVar MissingVariable | InvalidTemplate deriving Show
interpolateString :: [(VariableName, VariableValue)] -> String -> Either Error String


-- Section 4: N-queens problem
-- Queens and helpers.
-- range of a non-positive number is empty, range 3 is [0, 1, 2]
range :: Int -> [Int]
-- enumerate "foo" should return [(0, 'f'), (1, 'o'), (2, 'o')]
-- Hint: Use zip
enumerate :: [a] -> [(Int, a)]
-- Splits [1, 2, 3] should return [([1, 2, 3],[]), ([1, 2], [3]), ([1], [2, 3]), ([], [1, 2, 3]).
-- Order is important!
-- Hint: Splits [] is [([], [])].
splits :: [a] -> [([a], [a])]
-- permutations of [] is [[]]
-- permutations of [1, 2, 3] is [[1, 2, 3], [1, 3, 2], [2, 3, 1], [2, 1, 3], [3, 1, 2], [3, 2, 1]]
-- Hint: use splits
-- order is not important
permutations :: [a] -> [[a]]

type Column = Int
type Solution = [Column]
-- Returns all the solutions the n-queens problem. Returns a list of solutions, each solution made
-- up up of column per row. For example, queens 1 returns [[0]], queens 2 and queens 3 return [],
-- queens 4 returns [[1,3,0,2],[2,0,3,1]].
-- Order is not important.
queens :: Int -> [Solution]
-}
