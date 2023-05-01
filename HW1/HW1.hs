-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW1.hs should successfully compile.

-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}


module HW1 where

import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Int, Maybe (..), Num (..), Ord (..), Show (..), String, all, and, any, concat, concatMap, const, curry, elem, error, filter, flip, foldl, foldr, fst, id, length, lookup, map, not, notElem, null, or, product, reverse, snd, sum, uncurry, undefined, (!!), ($), (&&), (++), (.), (||))

-- Section 1: Utility functions
-- Basic Maybes
fromMaybe :: a -> Maybe a -> a
maybe :: b -> (a -> b) -> Maybe a -> b
catMaybes :: [Maybe a] -> [a]
mapMaybe :: (a -> Maybe b) -> [a] -> [b]

-- Basic Eithers
either :: (a -> c) -> (b -> c) -> Either a b -> c
mapLeft :: (a -> c) -> Either a b -> Either c b
catEithers :: [Either a b] -> Either a [b]
mapEither :: (a -> Either b c) -> [a] -> Either b [c]
concatEitherMap :: (b -> Either a c) -> Either a b -> Either a c
partitionEithers :: [Either a b] -> ([a], [b])


-- Section 2: Lists and zips
-- Fun with lists and zips
-- snoc is the opposite of cons, i.e., append to list.
snoc :: [a] -> a -> [a]
-- If one list is shorter than the other, take the shorter one, e.g.,
-- zipWith (+) [1, 2] [3, 4, 5] returns [4, 6]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- If one list is shorter than the other, take the shorter one, e.g.,
-- zip [1, 2] [3, 4, 5] returns [(1, 3), (2, 4)]
-- Could you implement this using zipWith and point-free style?
unzip :: [(a, b)] -> ([a], [b])
zip :: [a] -> [b] -> [(a, b)]


-- Section 3: String interpolation
-- Parsing template strings, e.g., "Hello ${name}!". See the PDF for more information.
splitOn :: Char -> String -> Maybe (String, String)

type Variable = String
data ParsedString = PlainString String | Variable String deriving Show
parseTemplate :: String -> Maybe [ParsedString]

type VariableName = String
type VariableValue = String
type MissingVariable = String
assignTemplate :: [(VariableName, VariableValue)] -> [ParsedString] -> Either MissingVariable String

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
