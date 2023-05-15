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


{-

>>> "bard" `isPrefixOf` "foobar"
False

>>> "oobo" `isInfixOf` "foobar"
False

>>> "bar" `isSuffixOf` "foobar"
True

>>> "oa" `isSubseuenceOf` "foobar"
True

>>> "ao" `isSubseuenceOf` "foobar"
False

-}


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


{-

>>> isDocumentMatchesQuery "abcd" (None [Literal "y", Literal "x"])
True

>>> findDocuments (Literal "ood") ["Hello!", "Goodbye!"] 
(["Goodbye!"],["Hello!"])

>>> findDocuments (All [Literal "a", Literal "c"]) ["abcd", "abef", "xyz", "abcdefg"]
(["abcd","abcdefg"],["abef","xyz"])

>>> findDocuments (Any [None [Literal "x"], All [Literal "ab", Literal "yz"]]) ["abcd", "abef", "xyz", "abcdefg"]
(["abcd","abef","abcdefg"],["xyz"])

>>> findDocuments (None [None [Literal "abc"]]) ["abcd", "abef", "xyz", "abcdefg"]
(["abcd","abcdefg"],["abef","xyz"])

-}


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


{-

>>> sample $ irepeat 1
[1,1,1,1,1,1,1,1,1,1]

>>> smallSample $ iiterate (\ x -> x * x + x) 1
[2,6,42,1806,3263442]

>>> sample $ icycle [1, 2, 3]
[1,2,3,1,2,3,1,2,3,1]

>>> sample naturals
[0,1,2,3,4,5,6,7,8,9]

>>> sample integers
[0,1,-1,2,-2,3,-3,4,-4,5]

>>> sample $ imap (* 3) naturals
[0,3,6,9,12,15,18,21,24,27]

>>> sample $ iscan (+) 0 naturals
[0,1,3,6,10,15,21,28,36,45]

>>> sample $ izip (imap (*3) naturals) (imap (*5) naturals)
[(0,0),(3,5),(6,10),(9,15),(12,20),(15,25),(18,30),(21,35),(24,40),(27,45)]

>>> sample $ interleave (imap (*3) naturals) (imap (*5) naturals)
[0,0,3,5,6,10,9,15,12,20]

>>> smallSample $ iinits naturals
[[],[0],[0,1],[0,1,2],[0,1,2,3]]

>>> smallSample $ imap smallSample $ itails naturals
[[1,2,3,4,5],[2,3,4,5,6],[3,4,5,6,7],[4,5,6,7,8],[5,6,7,8,9]]

-}


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

single :: a -> Tree a
single t = Tree EmptyTree t EmptyTree
tree :: Tree Int
tree = Tree (Tree (single 3) 2 (Tree (single 5) 4 EmptyTree)) 1 (Tree (Tree EmptyTree 7 (single 8)) 6 EmptyTree)


{-
>>> levelOrder tree
[1,2,6,3,4,7,5,8]

-}


{-

-- Bonus: if you don't wish to implement this, simply write ifind = undefined
ifind :: forall a. (a -> Bool) -> InfiniteList (InfiniteList a) -> Bool

fromListLevelOrder :: [a] -> Tree a

-}
