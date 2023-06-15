{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module PersistentArray (PersistentArray, lookup, set, empty, length, pushr) where

import Data.Array (Array)
import qualified Data.Array as A
import qualified GHC.Arr as Arr

import Prelude hiding (last, length, lookup)

type Vector = Array Int

data PersistentArray a
    = Leafs {v :: Vector a}
    | Node {h :: Int, l :: Int, vs :: Vector (PersistentArray a)}
    deriving Show

branchingFactor :: Int
branchingFactor = 32

length :: PersistentArray a -> Int
length Leafs{v} = Arr.numElements v
length Node{l} = l

emptyVector :: Vector a
emptyVector = A.listArray (1, 0) []

snoc :: Vector a -> a -> Vector a
snoc arr e = A.listArray (0, Arr.numElements arr) (A.elems arr ++ [e])

last :: Vector a -> a
last arr = arr Arr.! (Arr.numElements arr - 1)

singleton :: a -> Vector a
singleton = snoc emptyVector

empty :: PersistentArray a
empty = Leafs emptyVector

height :: PersistentArray a -> Int
height Leafs{} = 1
height Node{h} = h

isFull :: PersistentArray a -> Bool
isFull v = length v >= branchingFactor ^ height v

emptyWithHeight :: Int -> PersistentArray a
emptyWithHeight h =
    if h == 1 then empty else Node{h, l = 0, vs = emptyVector}

-- Utilities for updating a specific index. Vector only
-- supports bulk update using (//), which accepts a list (or
-- vector) of indices and new values.

-- Set the given index to a given value.
setIndex :: Int -> a -> Vector a -> Vector a
setIndex i a = (A.// [(i, a)])

-- Update the value at index using a given function.
modifyIndex :: Int -> (a -> a) -> Vector a -> Vector a
modifyIndex i f v = setIndex i (f $ v A.! i) v

modifyLastElement :: (a -> a) -> Vector a -> Vector a
modifyLastElement f v = modifyIndex (Arr.numElements v - 1) f v

pushr :: a -> PersistentArray a -> PersistentArray a
pushr e v =
    if isFull v
        then pushr e $ Node{h = height v + 1, l = length v, vs = singleton v}
        else case v of
            -- snoc is cons reversed, i.e., pushr.
            Leafs{v} -> Leafs $ v `snoc` e -- v isn't full
            Node{h, l, vs} ->
                let
                    updatedVector =
                        if null vs || isFull (last vs)
                            then vs `snoc` pushr e (emptyWithHeight $ h - 1)
                            else modifyLastElement (pushr e) vs
                 in
                    Node{h, l = l + 1, vs = updatedVector}

data TranslatedIndex a = TranslatedIndex
    { localIndex :: Int
    , branchIndex :: Int
    }

translateIndex :: -- Translate global index to a local one
    Int -> PersistentArray a -> TranslatedIndex a
translateIndex globalIndex Node{h} =
    let
        branchSize = branchingFactor ^ (h - 1)
        localIndex = globalIndex `mod` branchSize
        branchIndex = globalIndex `div` branchSize
     in
        TranslatedIndex{branchIndex, localIndex}
translateIndex _ _ = error "Should not have been called"

outOfBounds :: Int -> PersistentArray a -> Bool
outOfBounds n v = n < 0 || n >= length v

lookup :: Int -> PersistentArray a -> Maybe a
lookup n v =
    if outOfBounds n v
        then Nothing
        else case v of
            -- We already checked bounds
            Leafs{v} -> Just $ v A.! n
            Node{vs} ->
                let TranslatedIndex{localIndex, branchIndex} =
                        translateIndex n v
                 in lookup localIndex $ vs A.! branchIndex

set :: Int -> a -> PersistentArray a -> PersistentArray a
set n e v =
    if outOfBounds n v
        then v -- Could also raise an error, or return Nothing
        else case v of
            Leafs{v} -> Leafs $ setIndex n e v
            Node{h, l, vs} ->
                let TranslatedIndex{localIndex, branchIndex} =
                        translateIndex n v
                    updatedVector =
                        modifyIndex branchIndex (set localIndex e) vs
                 in Node{h, l, vs = updatedVector}
