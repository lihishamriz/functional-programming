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
guessingGame low high = do
    putStrLn ("Please pick a number between " ++ show low ++ " and " ++ show high)
    guessingGame' low high

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


{-

>>> dq1 = 1 `DQ.pushl` (2 `DQ.pushl` DQ.empty)
>>> dq2 = 30 `DQ.pushl` (40 `DQ.pushl` DQ.empty )
>>> toList $ liftA2 (*) dq1 dq2
[30,40,60,80]

>>> toList $ dq1 >>= (\x -> (x * 300) `DQ.pushl` (( x * 400) `DQ.pushl` DQ.empty ))
[300,400,600,800]

>>> ne1 = 1 :| [2]
>>> ne2 = 30 :| [40]
>>> toList $ liftA2 (*) ne1 ne2
[30,40,60,80]

>>> toList $ ne1 >>= (\x -> (x * 300) :| [x * 400 ])
[300,400,600,800]

>>> ne3 = 1 :| [2,3,4]
>>> ne4 = 10 :| [20]
>>> toList $ liftA2 (+) ne3 ne4
[11,21,12,22,13,23,14,24]

>>> toList $ ne3 >>= (\x -> (x * 10) :| [x * 100 ])
[10,100,20,200,30,300,40,400]

-}

list :: [(String, Expression)]
list =
  [ ("x", Literal 0)
  , ("z", Division (Literal 42) (Identifier "x"))
  , ("x", Plus (Literal 3) (Identifier "a"))
  -- x will be missing here, since it failed above.
  , ("y", Plus (Identifier "x") (Identifier "x"))
  , ("x", Plus (Literal 3) (Literal 4))
  , ("y", Plus (Identifier "x") (Identifier "x"))
  -- An assignment can reference itself.
  , ("x", Mult (Identifier "x") (Identifier "x"))
  ]

-- >>> runCalculator list
-- Result {finalValues = fromList [("x",49),("y",14)], missingVariables = fromList [("a",1),("x",1)], divisionByZero = 1}

list2 :: [(String, Expression)]
list2 = 
    [ ("x", Plus ( Literal 1) ( Literal 3) )
    , ("y", Division ( Mult ( Literal 3) ( Identifier "x")) ( Literal 2) )
    , ("x", Plus ( Literal 42) ( Identifier "y"))
    , ("z", Minus ( Mult ( Literal 4) ( Identifier "y")) ( Identifier "x") )
    ]

-- >>> runCalculator list2
-- Result {finalValues = fromList [("x",48),("y",6),("z",-24)], missingVariables = fromList [], divisionByZero = 0}