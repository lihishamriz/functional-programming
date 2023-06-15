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
fold = foldMap id

toList :: Foldable t => t a -> [a]
toList = foldMap (:[])

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = getAny . foldMap (Any . (==x))

find :: (Foldable t, Eq a) => (a -> Bool) -> t a -> Maybe a
find f = getFirst . foldMap (\x -> if f x then First (Just x) else First Nothing)

length :: Foldable t => t a -> Int
length = getSum . foldMap (\_ -> Sum 1)

null :: Foldable t => t a -> Bool
null = isNothing . getFirst . foldMap (\_ -> First (Just ()))

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = maxBy id

maxBy :: (Foldable t, Ord b) => (a -> b) -> t a -> Maybe a
maxBy f = foldr (\x res -> Just $ maybe x (\y -> if f x > f y then x else y) res) Nothing

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = minBy id

minBy :: (Foldable t, Ord b) => (a -> b) -> t a -> Maybe a
minBy f = foldr (\x res -> Just $ maybe x (\y -> if f x < f y then x else y) res) Nothing

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product


-- Section 2: Functor functions
fmapToFst :: Functor f => (a -> b) -> f a -> f (b, a)
fmapToFst f = fmap (\x -> (f x, x))

fmapToSnd :: Functor f => (a -> b) -> f a -> f (a, b)
fmapToSnd f = fmap (\x -> (x, f x))

strengthenL :: Functor f => b -> f a -> f (b, a)
strengthenL b = fmap (b,)

strengthenR :: Functor f => b -> f a -> f (a, b)
strengthenR b = fmap (,b)

unzip :: Functor f => f (a, b) -> (f a, f b)
unzip pairs = (fmap fst pairs, fmap snd pairs)

coUnzip :: Functor f => Either (f a) (f b) -> f (Either a b)
coUnzip = either (fmap Left) (fmap Right)


-- Section 3: Unfodlable
class Unfoldable t where
    fromList :: [a] -> t a
    fromList = unfoldr (\case
        [] -> Nothing
        (x : xs) -> Just (x, xs))

    unfoldr :: (b -> Maybe (a, b)) -> b -> t a
    unfoldr f = fromList . unfoldrList f where
        unfoldrList g b = case g b of
                Nothing -> []
                Just (a, b') -> a : unfoldrList g b'
                
    {-# MINIMAL fromList | unfoldr #-}

instance Unfoldable [] where
    fromList :: [a] -> [a]
    fromList xs = xs

instance Unfoldable Deque where
    unfoldr :: (b -> Maybe (a, b)) -> b -> Deque a
    unfoldr f = unfoldrDeque f DQ.empty where
        unfoldrDeque g dq b = case g b of
            Nothing -> dq
            Just (a, b') -> unfoldrDeque g (DQ.pushr a dq) b'

instance Unfoldable PersistentArray where
    fromList :: [a] -> PersistentArray a
    fromList x = foldr PA.pushr PA.empty $ reverse x


-- Section 4: Data structure instances
instance Foldable Deque where
    foldMap :: Monoid m => (a -> m) -> Deque a -> m
    foldMap f dq = case DQ.popl dq of
        Nothing -> mempty
        Just (x, dq') -> f x <> foldMap f dq'

instance Functor Deque where
    fmap :: (a -> b) -> Deque a -> Deque b
    fmap f dq = fromList $ fmap f (toList dq)

instance Semigroup (Deque a) where
    (<>) :: Deque a -> Deque a -> Deque a
    dq1 <> dq2 = fromList $ toList dq1 <> toList dq2

instance Monoid (Deque a) where
    mempty :: Deque a
    mempty = DQ.empty
    

instance Foldable PersistentArray where
    foldMap :: Monoid m => (a -> m) -> PersistentArray a -> m
    foldMap = go 0 where
        go i f arr = case PA.lookup i arr of
            Nothing -> mempty
            Just x  -> f x <> go (i + 1) f arr

instance Functor PersistentArray where 
    fmap :: (a -> b) -> PersistentArray a -> PersistentArray b
    fmap f arr = fromList $ fmap f (toList arr)

instance Semigroup (PersistentArray a) where
    (<>) :: PersistentArray a -> PersistentArray a -> PersistentArray a
    arr1 <> arr2 = fromList $ toList arr1 <> toList arr2

instance Monoid (PersistentArray a) where
    mempty :: PersistentArray a
    mempty = PA.empty


-- Bonus section
newtype ZipList a = ZipList {getZipList :: [a]} deriving (Show, Eq)

instance Semigroup a => Semigroup (ZipList a) where
    (<>) :: ZipList a -> ZipList a -> ZipList a
    ZipList xs <> ZipList ys = ZipList (zipWith (<>) xs ys)

instance Monoid a => Monoid (ZipList a) where
    mempty :: Monoid a => ZipList a
    mempty = ZipList $ unfoldr (\_ -> Just (mempty, ())) ()
