{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Needed for instance PrettyPrint [Statement]
{-# LANGUAGE FlexibleInstances #-}

-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
-- {-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW3 where

import qualified Data.Map as M
import Data.Map (Map, (!?))
import qualified Data.Set as S
import Data.Set (Set)
import Data.Either (either, fromLeft, fromRight, isLeft, isRight, lefts, partitionEithers, rights)
import Data.List (find, foldl', uncons)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybe)
import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Int, Maybe (..), Num (..), Ord (..), Ordering(..), Show (..), String, all, and, any, concat, concatMap, const, curry, drop, dropWhile, elem, error, filter, flip, foldl, foldr, fst, id, init, last, length, lookup, map, maximum, minimum, not, notElem, null, or, product, replicate, reverse, snd, sum, tail, take, takeWhile, uncurry, undefined, unlines, unzip, zip, zipWith, (!!), ($), (&&), (++), (.), (||))

type Variable = String
data Expression = Not Expression | Or Expression Expression | And Expression Expression | Var Variable | Literal Bool deriving (Show, Eq)
data Statement =
  Return Expression |
  Block [Statement] |
  If Expression [Statement] |
  IfElse Expression [Statement] [Statement] |
  Define Variable Expression
  deriving (Show, Eq)

-- Section 1.1: Pretty printing expressions
class PrettyPrint a where
  prettyPrint :: a -> String

instance PrettyPrint Expression where
  prettyPrint :: Expression -> String
  prettyPrint e = case e of
    Not x         -> "!" ++ addParenthesis x
    Or x y        -> prettyPrint x ++ " || " ++ addParenthesis y
    And x y       -> prettyPrint x ++ " && " ++ addParenthesis y
    Var v         -> v
    Literal True  -> "true"
    Literal False -> "false"

addParenthesis :: Expression -> String
addParenthesis e@(Or _ _) = "(" ++ prettyPrint e ++ ")"
addParenthesis e@(And _ _) = "(" ++ prettyPrint e ++ ")"
addParenthesis e = prettyPrint e

instance PrettyPrint Statement where
  prettyPrint :: Statement -> String
  prettyPrint s = prettyPrintWithIndentation 0 s

prettyPrintWithIndentation :: Int -> Statement -> String
prettyPrintWithIndentation n s = case s of
  Return x        -> getIndentation n ++ "return " ++ prettyPrint x
  Block xs        -> getIndentation n ++ "{\n" ++ block n xs ++ getIndentation n ++ "}"
  If e xs         -> getIndentation n ++ "if (" ++ prettyPrint e ++ ") {\n" ++ block n xs ++ getIndentation n ++ "}"
  IfElse e xs ys  -> prettyPrintWithIndentation n (If e xs) ++ " else {\n" ++ block n ys ++ getIndentation n ++ "}"
  Define v e      -> getIndentation n ++ v ++ " = " ++ prettyPrint e

getIndentation :: Int -> String
getIndentation 0 = ""
getIndentation n = "  " ++ getIndentation (n-1)

block :: Int -> [Statement] -> String
block n xs = unlines $ map (prettyPrintWithIndentation (n+1)) xs

instance PrettyPrint [Statement] where
  prettyPrint :: [Statement] -> String
  prettyPrint xs = unlines $ map prettyPrint xs


-- Section 1.2: Simplifying expressions and statements
type Scope = Map Variable Bool

simplifyExpression :: Scope -> Expression -> Expression
simplifyExpression scope e = case e of
  Not x -> case simplifyExpression scope x of
    Literal b -> Literal (not b)
    _ -> e

  Or x y -> case (simplifyExpression scope x, simplifyExpression scope y) of
    (Literal True, _) -> Literal True
    (_, Literal True) -> Literal True
    _ -> e
    
  And x y -> case (simplifyExpression scope x, simplifyExpression scope y) of
    (Literal False, _) -> Literal False
    (_, Literal False) -> Literal False
    _ -> e

  Var v -> case scope !? v of
    Just b  -> Literal b
    Nothing -> Var v
    
  Literal b -> Literal b

simplifyWithScope :: Scope -> [Statement] -> [Statement]
simplifyWithScope s = reverse . snd . foldl' (uncurry go) (s, []) where
  go :: Scope -> [Statement] -> Statement -> (Scope, [Statement])
  go scope statementsSoFar statement =
    let (newScope, simplified) = simplifyStatement scope statement
     in (newScope, simplified ++ statementsSoFar)
  simplifyStatement :: Scope -> Statement -> (Scope, [Statement])
  simplifyStatement scope statement = case statement of
    Return e -> (scope, [Return simplified]) where
      simplified = simplifyExpression scope e

    Define v e -> case simplifyExpression scope e of
      Literal b -> (newScope, [Define v newExp]) where
        newExp = simplifyExpression scope e
        newScope = M.insert v b scope
      _ -> (M.delete v scope, [Define v e])

    Block statements -> (scope, [Block simplified]) where
      simplified = simplifyWithScope scope statements

    If cond statements -> case simplifyExpression scope cond of
      Literal False -> (scope, [])
      Literal True  -> (scope, [Block $ simplifyWithScope scope statements])
      Var v         -> (scope, [If cond $ simplifyWithScope newScope statements]) where
        newScope = M.insert v True scope
      _ -> (scope, [If cond $ simplifyWithScope scope statements])

    IfElse cond ifBlock elseBlock -> case simplifyExpression scope cond of
      Literal False -> (scope, [Block $ simplifyWithScope scope elseBlock])
      Literal True  -> (scope, [Block $ simplifyWithScope scope ifBlock])
      Var v         -> (scope, [IfElse cond ifBlock' elseBlock']) where
        updateVar b = M.insert v b scope
        ifBlock' = simplifyWithScope (updateVar True) ifBlock
        elseBlock' = simplifyWithScope (updateVar False) elseBlock
      _ -> (scope, [IfElse cond (simplifyWithScope scope ifBlock) (simplifyWithScope scope elseBlock)])

simplify :: [Statement] -> [Statement]
simplify = simplifyWithScope M.empty


-- Section 2.1: Basic type classes
data Tree a = Empty | Tree (Tree a) a (Tree a)
instance Show a => Show (Tree a) where
  show :: Show a => Tree a -> String
  show t = "{" ++ go (inOrder t) ++ "}" where
    go = \case
      [] -> ""
      [x] -> show x
      (x : xs) -> show x ++ "," ++ go xs

instance Eq a => Eq (Tree a) where
  (==) :: Eq a => Tree a -> Tree a -> Bool
  (==) t1 t2 = inOrder t1 == inOrder t2

instance Ord a => Ord (Tree a) where
  (<=) :: Ord a => Tree a -> Tree a -> Bool
  (<=) t1 t2 = inOrder t1 <= inOrder t2

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Tree l x r) = inOrder l ++ [x] ++ inOrder r


-- Section 2.2: Typeclass constraints
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x : xs) = x : nub (filter (/=x) xs)

sort :: Ord a => [a] -> [a]
sort list = concat $ replicateKeys $ M.assocs $ tuplesToMap $ listToTuples list
  where
    replicateKeys [] = []
    replicateKeys ((x, y): xs) = replicate y x : replicateKeys xs
    tuplesToMap = M.fromListWith (+)
    listToTuples = map (\x -> (x, 1))

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f xs = argToValue $ sort $ valueToArg xs
  where
    argToValue = map (\(Arg _ v) -> v)
    valueToArg = map (\x -> Arg (f x) x)

data Arg a b = Arg a b
instance Eq a => Eq (Arg a b) where
  (Arg a1 _) == (Arg a2 _) = a1 == a2
instance Ord a => Ord (Arg a b) where
  (Arg a1 _) <= (Arg a2 _) = a1 <= a2
