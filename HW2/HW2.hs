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
import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Int, Maybe (..), Num (..), Ord (..), Show (..), String, all, and, any, concat, concatMap, const, curry, div, drop, dropWhile, elem, error, filter, flip, foldl, foldr, fst, id, init, last, length, lookup, map, maximum, minimum, mod, not, notElem, null, or, otherwise, product, reverse, snd, sum, tail, take, takeWhile, uncurry, undefined, unzip, zip, zipWith, (!!), ($), (&&), (++), (.), (||), Ordering (EQ))


-- Section 1: String matching
isPrefixOf :: String -> String -> Bool
isPrefixOf _ [] = False
isPrefixOf [] _ = True
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

isInfixOf :: String -> String -> Bool
isInfixOf _ [] = False
isInfixOf [] _ = True
isInfixOf xs ys@(_ : ys') = isPrefixOf xs ys || isInfixOf xs ys'

isSuffixOf :: String -> String -> Bool
isSuffixOf _ [] = False
isSuffixOf [] _ = True
isSuffixOf xs ys = reverse xs `isPrefixOf` reverse ys

isSubseuenceOf :: String -> String -> Bool
isSubseuenceOf _ [] = False
isSubseuenceOf [] _ = True
isSubseuenceOf xs@(x : xs') (y : ys') = if x == y then isSubseuenceOf xs' ys' else isSubseuenceOf xs ys'


-- Section 2: Document searching
type Phrase = String
data Query = All [Query] | Any [Query] | None [Query] | Literal Phrase deriving Show
type Document = String
findDocuments :: Query -> [Document] -> ([Document], [Document])
findDocuments q = foldr go ([], [])
    where
        go d (xs, ys) = if isDocumentMatchesQuery d q then (d : xs, ys) else (xs, d : ys)

isDocumentMatchesQuery :: Document -> Query -> Bool
isDocumentMatchesQuery d (Literal q) = q `isInfixOf` d
isDocumentMatchesQuery d (All qs) = all (isDocumentMatchesQuery d) qs
isDocumentMatchesQuery d (Any qs) = any (isDocumentMatchesQuery d) qs
isDocumentMatchesQuery d (None qs) = not (any (isDocumentMatchesQuery d) qs)


-- Section 3: InfiniteList
data InfiniteList a = a :> InfiniteList a
infixr 3 :>

sample :: InfiniteList a -> [a]
sample = take 10 . itoList

smallSample :: InfiniteList a -> [a]
smallSample = take 5 . itoList

itoList :: InfiniteList a -> [a]
itoList (x :> xs)  = x : itoList xs

irepeat :: a -> InfiniteList a
irepeat x = x :> irepeat x

iiterate :: (a -> a) -> a -> InfiniteList a
iiterate f x = f x :> iiterate f (f x)

icycle :: [a] -> InfiniteList a
icycle xs = go xs
    where
        go (x : xs') = x :> go xs'
        go [] = icycle xs

naturals :: InfiniteList Int
naturals = iiterate (+1) (-1)

integers :: InfiniteList Int
integers = go 0 1
    where
        go x y = if x == 0 then 0 :> go 1 (-1) else x :> y :> go (x+1) (y-1)

imap :: (a -> b) -> InfiniteList a -> InfiniteList b
imap f (x :> xs) = f x :> imap f xs

iscan :: (a -> b -> b) -> b -> InfiniteList a -> InfiniteList b
iscan f x (y :> ys) = f y x :> iscan f (f y x) ys

izip :: InfiniteList a -> InfiniteList b -> InfiniteList (a, b)
izip (x :> xs) (y :> ys) = (x, y) :> izip xs ys

interleave :: InfiniteList a -> InfiniteList a -> InfiniteList a
interleave (x :> xs) (y :> ys) = x :> y :> interleave xs ys

iinits :: InfiniteList a -> InfiniteList [a]
iinits xs = go 0
    where
        go n = take n (itoList xs) :> go (n+1)

itails :: InfiniteList a -> InfiniteList (InfiniteList a)
itails (_ :> xs) = xs :> itails xs

-- Bonus
ifind :: forall a. (a -> Bool) -> InfiniteList (InfiniteList a) -> Bool
ifind f = go 1
  where
    go i xss = go' i (take i (itoList xss)) || go (i + 1) xss

    go' _ [] = False
    go' i (ys : yss) = go'' i 1 ys || go' i yss

    go'' i j ys | j > i = go'' i (j + 1) ys
    go'' _ j ys = case find f (take j (itoList ys)) of
        Just _ -> True
        Nothing -> False


-- Section 4: Binary trees (no necessarily search trees)
data Tree a = EmptyTree | Tree (Tree a) a (Tree a) deriving Show

preOrder :: Tree a -> [a]
preOrder EmptyTree = []
preOrder (Tree l x r) = [x] ++ preOrder l ++ preOrder r

inOrder :: Tree a -> [a]
inOrder EmptyTree = []
inOrder (Tree l x r) = inOrder l ++ [x] ++ inOrder r

postOrder :: Tree a -> [a]
postOrder EmptyTree = []
postOrder (Tree l x r) = postOrder l ++ postOrder r ++ [x]

levelOrder :: Tree a -> [a]
levelOrder a = levelOrder' [a]
  where
    levelOrder' [] = []
    levelOrder' (EmptyTree : xs) = levelOrder' xs
    levelOrder' (Tree l x r: xs) = x : levelOrder' (xs ++ [l, r])

fromListLevelOrder :: [a] -> Tree a
fromListLevelOrder [] = EmptyTree
fromListLevelOrder xs = go 1
    where
        go i | i > length xs = EmptyTree
        go i = Tree (go (2 * i)) (xs !! (i - 1)) (go (2 * i + 1))