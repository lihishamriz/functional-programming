{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wall -Werror #-}
-- Needed for adding instances outside the data definition module.
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW4.HW4 where

import Data.Either
import Data.List (foldl', uncons)
import Data.Map (Map, (!?))
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid (All (..), Any (..), First (..), Last (..), Product (..), Sum (..))
import Data.Ord (Down(..))
import Data.Semigroup (Arg (..), Max (..), Min (..))
import Data.Set (Set)
import qualified Data.Set as S
import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Foldable (foldMap, foldl, foldr), Functor (fmap), Int, Maybe (..), Monoid (..), Num (..), Ord (..), Ordering (..), Semigroup (..), Show (..), String, all, and, any, concat, concatMap, const, curry, drop, dropWhile, error, filter, flip, fst, id, init, map, not, or, replicate, reverse, snd, take, takeWhile, uncurry, undefined, zip, zipWith, (!!), ($), (&&), (++), (.), (||))

import HW4.Deque (Deque)
import qualified HW4.Deque as DQ
import HW4.PersistentArray (PersistentArray)
import qualified HW4.PersistentArray as PA

-- Section 1: Foldable functions
fold :: (Foldable t, Monoid a) => t a -> a
fold = foldMap id

toList :: Foldable t => t a -> [a]
toList = foldMap (:[])

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = getAny . foldMap (Any . (==x))

find :: (Foldable t, Eq a) => (a -> Bool) -> t a -> Maybe a
find f = getFirst . foldMap (\x -> if f x then First (Just x) else First Nothing)

length :: Foldable t => t a -> Int
length = getSum . foldMap (\_ -> Sum 1)

-- null :: Foldable t => t a -> Bool

{-

>>> getSum $ fold $ map Sum [1, 2, 3]
6

>>> toList $ Just 4
[4]

>>> toList $ Nothing
[]

>>> elem 3 [1,2,3]
True

>>> elem 4 [1,2,3]
False

>>> elem 'e' "hello"
True

>>> elem 'a' "hello"
False

>>> find (==1) $ Tree ( single 1) 2 ( single 3)
Just 1

>>> find (== (Arg 1 "")) [Arg 1 "a", Arg 1 "b", Arg 1 "c"]
Just (Arg 1 "a")

>>> find (<0) [1,2,3]
Nothing

>>> length [1,2,3]
3

>>> length "abc"
3

>>> length []
0

>>> length ""
0

>>> null []
True

>>> null ""
True

>>> null [1,2,3]
False

-}

{-

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maxBy :: (Foldable t, Ord b) => (a -> b) -> t a -> Maybe a
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minBy :: (Foldable t, Ord b) => (a -> b) -> t a -> Maybe a
sum :: (Foldable t, Num a) => t a -> a
product :: (Foldable t, Num a) => t a -> a

-- Section 2: Functor functions
fmapToFst :: Functor f => (a -> b) -> f a -> f (b, a)
fmapToSnd :: Functor f => (a -> b) -> f a -> f (a, b)
strengthenL :: Functor f => b -> f a -> f (b, a)
strengthenR :: Functor f => b -> f a -> f (a, b)
unzip :: Functor f => f (a, b) -> (f a, f b)
coUnzip :: Functor f => Either (f a) (f b) -> f (Either a b)

-- Section 3: Unfodlable
class Unfoldable t where
    fromList :: [a] -> t a
    unfoldr :: (b -> Maybe (a, b)) -> b -> t a
    {-# MINIMAL fromList | unfoldr #-}

instance Unfoldable []
instance Unfoldable Deque
instance Unfoldable PersistentArray

-- Section 4: Data structure instances
instance Foldable Deque
instance Functor Deque
instance Semigroup (Deque a)
instance Monoid (Deque a)

instance Foldable PersistentArray
instance Functor PersistentArray
instance Semigroup (PersistentArray a)
instance Monoid (PersistentArray a)

-- Bonus section
newtype ZipList a = ZipList {getZipList :: [a]} deriving (Show, Eq)

instance Semigroup a => Semigroup (ZipList a)
instance Monoid a => Monoid (ZipList a)

-}

-- TESTS
data Tree a = Empty | Tree (Tree a) a (Tree a) deriving Show

single :: a -> Tree a
single a = Tree Empty a Empty


instance Foldable Tree where
    foldr :: ( a -> b -> b ) -> b -> Tree a -> b
    foldr _ b Empty = b
    foldr f b ( Tree l a r ) = -- Notice the order !
        let rAgg = foldr f b r
            currentAgg = f a rAgg
            in foldr f currentAgg l

-- dequeFromList :: Deque Int
-- dequeFromList = fromList [1, 2, 3, 4]

-- dequeFromUnfoldr :: Deque Int
-- dequeFromUnfoldr = unfoldr (\x -> if x < 10 then Just (x,x*2) else Nothing) 1


-- arrayFromList :: PersistentArray Int
-- arrayFromList = fromList [1, 2, 3, 4]

-- arrayFromUnfoldr :: PersistentArray Int
-- arrayFromUnfoldr = unfoldr (\x -> if x < 10 then Just (x,x*2) else Nothing) 1

dq1 :: Deque Int
dq1 = DQ.pushl 1 $ DQ.pushr 2 DQ.empty
dq2 :: Deque Int
dq2 = DQ.pushl 3 $ DQ.pushr 4 DQ.empty

array1 :: PersistentArray Int
array1 = PA.pushr 2 $ PA.pushr 1 PA.empty
array2 :: PersistentArray Int
array2 = PA.pushr 4 $ PA.pushr 3 PA.empty
