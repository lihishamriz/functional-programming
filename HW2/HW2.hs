{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW2.hs should successfully compile.

-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW2 where

import Data.Either (either, fromLeft, fromRight, isLeft, isRight, lefts, partitionEithers, rights)
import Data.List (find, foldl', uncons)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybe)
import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Int, Maybe (..), Num (..), Ord (..), Show (..), String, all, and, any, concat, concatMap, const, curry, div, drop, dropWhile, elem, error, filter, flip, foldl, foldr, fst, id, init, last, length, lookup, map, maximum, minimum, mod, not, notElem, null, or, otherwise, product, reverse, snd, sum, tail, take, takeWhile, uncurry, undefined, unzip, zip, zipWith, (!!), ($), (&&), (++), (.), (||))


-- Section 1: String matching
isPrefixOf :: String -> String -> Bool
isInfixOf :: String -> String -> Bool
isSuffixOf :: String -> String -> Bool
isSubseuenceOf :: String -> String -> Bool


-- Section 2: Document searching
type Phrase = String
data Query = All [Query] | Any [Query] | None [Query] | Literal Phrase deriving Show
type Document = String
findDocuments :: Query -> [Document] -> ([Document], [Document])


-- Section 3: InfiniteList
data InfiniteList a = a :> InfiniteList a
infixr 3 :>

itoList :: InfiniteList a -> [a]

irepeat :: a -> InfiniteList a
iiterate :: (a -> a) -> a -> InfiniteList a
icycle :: [a] -> InfiniteList a

naturals :: InfiniteList Int
integers :: InfiniteList Int

imap :: (a -> b) -> InfiniteList a -> InfiniteList b
iscan :: (a -> b -> b) -> b -> InfiniteList a -> InfiniteList b

izip :: InfiniteList a -> InfiniteList b -> InfiniteList (a, b)
interleave :: InfiniteList a -> InfiniteList a -> InfiniteList a

iinits :: InfiniteList a -> InfiniteList [a]
itails :: InfiniteList a -> InfiniteList (InfiniteList a)

-- Bonus: if you don't wish to implement this, simply write ifind = undefined
ifind :: forall a. (a -> Bool) -> InfiniteList (InfiniteList a) -> Bool


-- Section 4: Binary trees (no necessarily search trees)
data Tree a = EmptyTree | Tree (Tree a) a (Tree a) deriving Show
preOrder :: Tree a -> [a]
postOrder :: Tree a -> [a]
inOrder :: Tree a -> [a]
levelOrder :: Tree a -> [a]
fromListLevelOrder :: [a] -> Tree a
