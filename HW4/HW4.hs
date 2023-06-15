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

{-

>>> getSum $ fold $ map Sum [1, 2, 3]
6

>>> toList $ Just 4
[4]

>>> toList $ Nothing
[]

>>> toList $ Tree (single 1) 2 (single 3)
[1,2,3]

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

>>> null (Nothing)
True

>>> maximum [1,5,2]
Just 5

>>> maximum []
Nothing

>>> maximum [-2,0,-3]
Just 0

>>> maxBy length ["foo", "bar", "bazz"]
Just "bazz"

>>> minimum [1,5,2]
Just 1

>>> minimum []
Nothing

>>> minimum [-2,0,-3]
Just (-3)

>>> minBy length ["foo", "bar", "bazz"]
Just "bar"

>>> sum [1,3,4]
8

>>> sum (Just 2)
2

>>> product [1,3,9]
27

-}


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

{-

>>> fmapToFst length ["foo", "bar"]
[(3,"foo"),(3,"bar")]

>>> fmapToSnd length $ Just "foo" 
Just ("foo",3)

>>> strengthenL 42 $ Right "foo"
Right (42,"foo")

>>> strengthenR "x" [1, 2, 3]
[(1,"x"),(2,"x"),(3,"x")]

>>> unzip $ Just (1 ,2)
(Just 1,Just 2)

>>> coUnzip (Right [1,2,3] :: Either String [Int])
[Right 1,Right 2,Right 3]

>>> coUnzip (Left "foo" :: Either String [Int])
[Left 'f',Left 'o',Left 'o']

-}


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

{-

>>> fromList [1, 2, 3] :: [Int]
[1,2,3]

>>> take 5 $ fromList [1..] :: [Int]
[1,2,3,4,5]

>>> unfoldr (\ x -> if x > 5 then Just (x, x + 1) else Nothing) 1 :: [Int]
[]

>>> unfoldr (\ x -> if x <= 5 then Just (x, x + 1) else Nothing) 1 :: [Int]
[1,2,3,4,5]

>>> take 5 $ unfoldr (\ x -> Just (x, x + 1)) 1 :: [Int]
[1,2,3,4,5]

>>> map fst [dq3, dq4, dq5] 
[1,2,3]

>>> dequeFromUnfoldr
Deque [] [8,4,2,1]

>>> [PA.lookup 0 arrayFromList , PA.lookup 1 arrayFromList , PA.lookup 2 arrayFromList]
[Just 1,Just 2,Just 3]

>>> [PA.lookup 0 arrayFromUnfoldr , PA.lookup 1 arrayFromUnfoldr , PA.lookup 3 arrayFromUnfoldr]
[Just 1,Just 2,Just 8]

-}


-- Section 4: Data structure instances
instance Foldable Deque where
    foldMap :: Monoid m => (a -> m) -> Deque a -> m
    foldMap f dq = case DQ.popl dq of
        Nothing -> mempty
        Just (x, dq') -> f x <> foldMap f dq'

instance Functor Deque where
    fmap :: (a -> b) -> Deque a -> Deque b
    fmap f dq = fromList $ map f (toList dq)

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

{-

>>> toList $ dq7 <> dq8
[1,2,3,4]

>>> toList $ array1 <> array2
[1,2,3,4]

-}


-- Bonus section
newtype ZipList a = ZipList {getZipList :: [a]} deriving (Show, Eq)

instance Semigroup a => Semigroup (ZipList a) where
    (<>) :: ZipList a -> ZipList a -> ZipList a
    ZipList xs <> ZipList ys = ZipList (zipWith (<>) xs ys)

instance Monoid a => Monoid (ZipList a) where
    mempty :: Monoid a => ZipList a
    mempty = ZipList $ unfoldr (\_ -> Just (mempty, ())) ()

{-

>>> map getSum $ getZipList $ ZipList (map Sum [1, 2, 3]) <> ZipList (map Sum [4, 5])
[5,7]

>>> take 5 $ map getProduct $ getZipList $ ZipList (map Product [1..]) <> ZipList (map Product [0..])
[0,2,6,12,20]

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

dequeFromList :: Deque Int
dequeFromList = fromList [1, 2, 3, 4]

dequeFromUnfoldr :: Deque Int
dequeFromUnfoldr = unfoldr (\x -> if x < 10 then Just (x,x*2) else Nothing) 1

arrayFromList :: PersistentArray Int
arrayFromList = fromList [1, 2, 3, 4]

arrayFromUnfoldr :: PersistentArray Int
arrayFromUnfoldr = unfoldr (\x -> if x < 10 then Just (x,x*2) else Nothing) 1

dq3 :: (Int, Deque Int)
dq3 = fromJust $ DQ.popl dequeFromList 
dq4 :: (Int, Deque Int)
dq4 = fromJust $ DQ.popl $ snd dq3
dq5 :: (Int, Deque Int)
dq5 = fromJust $ DQ.popl $ snd dq4

dq7 :: Deque Int
dq7 = DQ.pushl 1 $ DQ.pushr 2 DQ.empty
dq8 :: Deque Int
dq8 = DQ.pushl 3 $ DQ.pushr 4 DQ.empty

array1 :: PersistentArray Int
array1 = PA.pushr 2 $ PA.pushr 1 PA.empty
array2 :: PersistentArray Int
array2 = PA.pushr 4 $ PA.pushr 3 PA.empty
