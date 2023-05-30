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

import System.IO (putStrLn)
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
  prettyPrint (Not x) = "!" ++ addParenthesis x
  prettyPrint (Or x y) = prettyPrint x ++ " || " ++ addParenthesis y
  prettyPrint (And x y) = prettyPrint x ++ " && " ++ addParenthesis y
  prettyPrint (Var v) = v
  prettyPrint (Literal True) = "true"
  prettyPrint (Literal False) = "false"

addParenthesis :: Expression -> String
addParenthesis e@(Or x y) = "(" ++ prettyPrint e ++ ")"
addParenthesis e@(And x y) = "(" ++ prettyPrint e ++ ")"
addParenthesis x = prettyPrint x

{-

>>> prettyPrint $ Not $ Var "x"
"!x"

>>> prettyPrint $ Or ( Var "x") ( Var "y")
"x || y"

>>> prettyPrint $ And ( Var "x") ( Not $ Var "y")
"x && !y"

>>> prettyPrint $ Or ( Var "x") ( Not $ And ( Var "y") ( Not $ Var "z"))
"x || !(y && !z)"

-}

instance PrettyPrint Statement where
  prettyPrint :: Statement -> String
  prettyPrint s = prettyPrintIndentation 0 s


prettyPrintIndentation :: Int -> Statement -> String
prettyPrintIndentation n (Return x) = indentation n ++ "return " ++ prettyPrint x
prettyPrintIndentation n (Block xs) = indentation n ++ "{\n" ++ block n xs ++ indentation n ++ "}"
prettyPrintIndentation n (If e xs) = indentation n ++ "if (" ++ prettyPrint e ++ ") {\n" ++ block n xs ++ indentation n ++ "} "
prettyPrintIndentation n (IfElse e xs ys) = prettyPrintIndentation n (If e xs) ++ "else {\n" ++ block n xs ++ indentation n ++ "} "
prettyPrintIndentation n (Define v e) = indentation n ++ v ++ " = " ++ prettyPrint e

indentation :: Int -> String
indentation 0 = ""
indentation n = "  " ++ indentation (n-1)

block :: Int -> [Statement] -> String
block n xs = unlines (map (prettyPrintIndentation (n+1)) xs)

p :: Statement
p = If ( Var "x") [ Return $ Literal True ]

s :: Statement
s = If ( And ( Var "x") ( Not $ Var "y")) [ Define "z" ( Var "x") , Block [ Define "y" ( Literal False ) , Define "y" ( And ( Var "y") ( Var "z")) ], Define "a" ( Or ( Var "z") ( Var "y")) , Block [], IfElse ( Var "a") [ Return $ Literal True ] [ Return $ Var "z"] ]

{-

>>> prettyPrint p
"if (x) {\n  return true\n}\n"

>>> prettyPrint s
"if (x && !y) {\n  z = x\n  {\n    y = false\n    y = y && z\n  }\n\n  a = z || y\n  {\n  }\n\n  if (a) {\n    return true\n  }\nelse {\n    return true\n  }\n\n}\n"

>>> putStrLn (prettyPrint s)

-}

instance PrettyPrint [Statement] where
  prettyPrint :: [Statement] -> String
  prettyPrint [] = ""
  prettyPrint (x:xs) = prettyPrint x ++ "\n" ++ prettyPrint xs


-- Section 1.2: Simplifying expressions and statements
type Scope = Map Variable Bool

simplifyExpression :: Scope -> Expression -> Expression
simplifyExpression scope (Var v) = case scope !? v of
  Just b -> Literal b
  Nothing -> Var v

simplifyExpression scope e@(Not x) = case simplifyExpression scope x of
  Literal b -> Literal (not b)
  _ -> e

simplifyExpression scope e@(Or x y) = case (simplifyExpression scope x, simplifyExpression scope y) of
  (Literal True, _) -> Literal True
  (_, Literal True) -> Literal True
  _ -> e

simplifyExpression scope e@(And x y) = case (simplifyExpression scope x, simplifyExpression scope y) of
  (Literal False, _) -> Literal False
  (_, Literal False) -> Literal False
  _ -> e

simplifyExpression _ (Literal x) = Literal x

simplifyWithScope :: Scope -> [Statement] -> [Statement]
simplifyWithScope s = reverse . snd . foldl' (uncurry go) (s, []) where
  go :: Scope -> [Statement] -> Statement -> (Scope, [Statement])
  go scope statementsSoFar statement =
    let (newScope, simplified) = simplifyStatement scope statement
     in (newScope, simplified ++ statementsSoFar)
  simplifyStatement :: Scope -> Statement -> (Scope, [Statement])

  simplifyStatement scope (Return e) =
    let simplified = simplifyExpression scope e
    in (scope, [Return simplified])

  simplifyStatement scope s@(Define v e) = case simplifyExpression scope e of
    (Literal b) -> (newScope, [Define v newExp]) where
      newExp = simplifyExpression scope e
      newScope = M.insert v b scope
    _ -> (M.delete v scope, [s])

  simplifyStatement scope (Block statements) =
    let simplified = simplifyWithScope scope statements
    in (scope, [Block simplified])

  simplifyStatement scope s@(If cond statements) = case simplifyExpression scope cond of
    (Literal False) -> (scope, [])
    (Literal True) -> (scope, [Block $ simplifyWithScope scope statements])
    (Var v) ->
      let newScope = M.insert v True scope
      in (scope, [If cond (simplifyWithScope newScope statements)])
    _ -> (scope, [If cond (simplifyWithScope scope statements)])

  simplifyStatement scope s@(IfElse cond ifBlock elseBlock) = case simplifyExpression scope cond of
    (Literal False) -> (scope, [Block $ simplifyWithScope scope elseBlock])
    (Literal True) -> (scope, [Block $ simplifyWithScope scope ifBlock])
    (Var v) -> (scope, [IfElse cond ifBlock' elseBlock']) where
      updateVar b = M.insert v b scope
      ifBlock' = simplifyWithScope (updateVar True) ifBlock
      elseBlock' = simplifyWithScope (updateVar False) elseBlock
    _ -> (scope, [IfElse cond (simplifyWithScope scope ifBlock) (simplifyWithScope scope elseBlock)])

simplify :: [Statement] -> [Statement]
simplify = simplifyWithScope M.empty

q :: [Statement]
q = [If (Var "x") [Define "z" (Or (Var "x") (Var "y")), Block [If (Var "z") [Define "y" (Not (Var "z"))], Define "z" (Var "y"), IfElse (Var "z") [Return (Literal False)] [Return (Var "z")]]]]

{-

>>> putStrLn $ prettyPrint $ simplify statements

-}

-- Section 2.1: Basic type classes
data Tree a = Empty | Tree (Tree a) a (Tree a)
instance Show a => Show (Tree a) where
  show :: Show a => Tree a -> String
  show t = "{" ++ go (inOrder t) ++ "}" where
    go = \case
      [] -> ""
      [x] -> show x
      (x : y : xs) -> show x ++ "," ++ go (y: xs)

instance Eq a => Eq (Tree a) where
  (==) :: Eq a => Tree a -> Tree a -> Bool
  (==) t1 t2 = inOrder t1 == inOrder t2

instance Ord a => Ord (Tree a) where
  (<=) :: Ord a => Tree a -> Tree a -> Bool
  (<=) t1 t2 = inOrder t1 <= inOrder t2

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Tree l x r) = inOrder l ++ [x] ++ inOrder r

single e = Tree Empty e Empty
tree1 = Tree Empty 1 $ Tree Empty 2 $ single 3
tree2 = Tree (single 1) 2 (single 3)

{-

>>> tree1 == tree2
True

>>> show tree1
"{1,2,3}"

>>> show tree2
"{1,2,3}"

>>> tree1 `compare` tree2
EQ

-}

-- Section 2.2: Typeclass constraints
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x : xs) = x : nub (filter (/=x) xs)

sort :: Ord a => [a] -> [a]
sort xs = concat (go (M.assocs (createMap (listToTuples xs))))

go :: [(a, Int)] -> [[a]]
go [] = []
go ((x, y): xs) = replicate y x : go xs

createMap :: (Num a, Ord k) => [(k, a)] -> M.Map k a
createMap = M.fromListWith (+)

listToTuples = map (\x -> (x, 1))

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f xs = map (\(Arg _ v) -> v) sortedArgList
  where
    sortedArgList = sort argList
    argList = map (\x -> Arg (f x) x) xs

data Arg a b = Arg a b
instance Eq a => Eq (Arg a b) where
  (Arg a1 _) == (Arg a2 _) = a1 == a2
instance Ord a => Ord (Arg a b) where
  (Arg a1 _) <= (Arg a2 _) = a1 <= a2

data Person = Person Int String deriving Show
age (Person a _)=a
name (Person _ n) = n
persons = [Person 40 "Taylor", Person 42 "Isaac", Person 37 "Zac"]

{-

>>> nub [2,1,3,3,4,1,2]
[2,1,3,4]

>>> sort [3, 1, 2, 3]
[1,2,3,3]

>>> sortOn (Down . name) persons
[Person 37 "Zac",Person 40 "Taylor",Person 42 "Isaac"]

>>> sortOn (Down . age) persons
[Person 42 "Isaac",Person 40 "Taylor",Person 37 "Zac"]

-}