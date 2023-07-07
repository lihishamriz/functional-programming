{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wall -Werror #-}
-- Needed for adding instances outside the data definition module.
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW5 where

import Control.Applicative (liftA2)
import Control.Monad
import Data.Either
import Data.Foldable
import Data.List (foldl', uncons)
import Data.Map (Map, (!?))
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid (All (..), Any (..), First (..), Last (..), Product (..), Sum (..))
import Data.Ord (Down (..))
import Data.Semigroup (Arg (..), Max (..), Min (..))
import Data.Set (Set)
import qualified Data.Set as S
import Prelude (Applicative (..), Bool (..), Char, Either (..), Enum (..), Eq (..), FilePath, Foldable (foldMap, foldl, foldr), Functor (fmap), IO, Int, Maybe (..), Monad (..), Monoid (..), Num (..), Ord (..), Ordering (..), Semigroup (..), Show (..), String, all, and, any, concat, concatMap, const, curry, div, drop, dropWhile, error, filter, flip, fst, getLine, id, init, lines, map, not, or, otherwise, putStrLn, readFile, replicate, reverse, sequenceA, snd, take, takeWhile, traverse, uncurry, undefined, unlines, writeFile, zip, zipWith, (!!), ($), (&&), (++), (.), (<$>), (||))

import Calculator
import Deque (Deque)
import qualified Deque as DQ
import State


-- Section 1
data NonEmpty a = a :| [a] deriving (Show, Eq, Ord, Functor, Foldable)
instance Semigroup (NonEmpty a) where
    (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
    (x :| xs) <> (y :| ys) = x :| (xs ++ y : ys)

instance Applicative NonEmpty where
    pure :: a -> NonEmpty a
    pure = return

    liftA2 :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
    liftA2 f a b = do
        x <- a
        f x <$> b

instance Monad NonEmpty where
    (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
    (x :| xs) >>= f = let (y :| ys) = f x
        in y :| (ys ++ (xs >>= toList . f))

instance Applicative Deque where
    pure :: a -> Deque a
    pure a = DQ.pushl a DQ.empty

    liftA2 :: (a -> b -> c) -> Deque a -> Deque b -> Deque c
    liftA2 f fa fb = fa >>= (\a -> f a <$> fb )

instance Monad Deque where
    (>>=) :: Deque a -> (a -> Deque b) -> Deque b
    (>>=) dq f = foldMap f dq


-- Section 2
joinGrades :: FilePath -> FilePath -> FilePath -> IO ()
joinGrades f1 f2 f3 = do
    s1 <- readFile f1
    s2 <- readFile f2
    writeFile f3 $ joinGradesStrings s1 s2

joinGradesStrings :: String -> String -> String
joinGradesStrings s1 s2 = go s
    where
        s = split <$> lines s1
        go [] = ""
        go (x:xs) = fst x ++ "," ++ searchGrade (snd x) s2 ++ "\n" ++ go xs

searchGrade :: String -> String -> String
searchGrade s1 s2 = go s 
    where
        s = split <$> lines s2
        go [] = "0"
        go (x:xs) = if fst x == s1 then snd x else go xs

split :: String -> (String, String)
split = go [] 
    where
        go res [] = (reverse res, [])
        go res (',':xs) = (reverse res, xs)
        go res (x:xs) = go (x:res) xs


-- Section 3
guessingGame :: Int -> Int -> IO Int
guessingGame minN maxN = do
    putStrLn ("Please pick a number between " ++ show minN ++ " and " ++ show maxN)
    guessingGame' minN maxN

guessingGame' :: Int -> Int -> IO Int
guessingGame' minN maxN
  | minN == maxN = return minN
  | maxN < minN = return 0
  | otherwise = do
        let guess = (minN + maxN) `div` 2
        putStrLn $ "Is the number less than , equal , or greater than " ++ show guess ++ "? (l/e/g)"
        answer <- getLine
        case answer of
            "l" -> guessingGame' minN (guess - 1)
            "g" -> guessingGame' (guess + 1) maxN
            "e" -> return guess
            _ -> return 0


-- Section 4
data Result = Result
    { finalValues :: Map String Int
    , missingVariables :: Map String Int
    , divisionByZero :: Int
    }
    deriving (Show, Eq)

runCalculator :: [(String, Expression)] -> Result
runCalculator xs = execState (traverse runable xs) (Result M.empty M.empty 0)

runable :: (String, Expression) -> State Result ()
runable (var, exp) = do
    state <- get
    
    case evaluate (finalValues state) exp of
        Left (MissingIdentifier v) -> modify $ \s -> s { 
            finalValues = M.delete var (finalValues s),
            missingVariables = M.insertWith (+) v 1 (missingVariables s)}

        Left DivisionByZero -> modify $ \s -> s { 
            finalValues = M.delete var (finalValues s),
            divisionByZero = divisionByZero s + 1}
        
        Right value -> modify $ \s -> s { finalValues = M.insert var value (finalValues s)}
