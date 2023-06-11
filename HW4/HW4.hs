{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wall -Werror #-}
-- Needed for adding instances outside the data definition module.
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW4 where

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

import Deque (Deque)
import qualified Deque as DQ
import PersistentArray (PersistentArray)
import qualified PersistentArray as PA

-- Section 1: Foldable functions
fold :: (Foldable t, Monoid a) => t a -> a
toList :: Foldable t => t a -> [a]
elem :: (Foldable t, Eq a) => a -> t a -> Bool
find :: (Foldable t, Eq a) => (a -> Bool) -> t a -> Maybe a
length :: Foldable t => t a -> Int
null :: Foldable t => t a -> Bool
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
