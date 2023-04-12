-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW0.hs should successfully compile.
--
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}

module HW0 where

-- These import statement ensures you aren't using any "advanced" functions and types, e.g., lists.
import Prelude (Bool (..), Eq (..), Int, Integer, Num (..), Ord (..), div, error, flip, id, mod, ($), (&&), (.), (||))

-- For a bit of an extra challange, use the below import list instead, and implement the missing functions yourselves!
-- import Prelude (Num(..), Bool(..), Int, Integer, Eq(..), Ord(..), div, error)

-- ************************************************************** --
-- Higher order functions. Most of these should be self-explanatory.
-- ************************************************************** --

const :: a -> b -> a
(.>) :: (a -> b) -> (b -> c) -> a -> c
curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
rotate :: (a -> b -> c -> d) -> c -> a -> b -> d
lotate :: (a -> b -> c -> d) -> b -> c -> a -> d

-- or (> 0) (< 0) 5 returns True
-- or (> 0) (< 0) 0 returns False
or :: (a -> Bool) -> (a -> Bool) -> a -> Bool
-- and (> 0) (< 0) 5 returns False
-- and (> 0) (> 10) 20 returns True
and :: (a -> Bool) -> (a -> Bool) -> a -> Bool
-- Did you implement the above two without duplication?

-- Applies the function until some condition holds.
-- until (+1) (> 0) 5 returns 5
-- until (*2) (> 10) 1 returns 16
-- until (*1) (> 0) 0 would never terminate!
until :: (a -> a) -> (a -> Bool) -> a -> a

-- ******************************** --
-- Numerical functions and algorithms.
-- ******************************** --

-- Returns the greatest common divisor (GCD) of two numbers. The GCD is always a positive number.
-- gcd 123456 432 is 48.
gcd :: Int -> Int -> Int
-- Returns the least common multiplier (LCM) of two numbers. The LCM is always a positive number.
-- lcm 123456 432 is 1111104
-- Hint: you can use gcd, or implement it recursively.
lcm :: Int -> Int -> Int

-- Returns the length of the Collatz sequence. The Collatz function is defined thus:
-- collatz of 1 is 1.
-- collatz of an even number is the number divided by 2.
-- collatz of an uneven number greater than 1 is collatz of 3 * n + 1.
-- A collatz sequence, is the sequence of element until the number reaches 1.
-- For example, for 13, the sequence is 13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1.
-- The length of the sequence is 10.
-- Write a function which computes the length of the sequence for a given positive integer n.
-- collatzSequence 13 is 10.
-- collatzSequence of 0 or -13 is 0.
collatzSequence :: Int -> Int

-- Count the number of digits in an integer. For example, countDigits 123 should return 3.
-- A negative number has the same number of digits as its absolute value, so countDigits (-123)
-- is also 3.
countDigits :: Integer -> Int
-- Returns the number reversed. For example, reverseNum of 123 is 321. The reverse of a negative
-- number is also negative, so negativeNum (-123) is -321.
reverseNum :: Integer -> Integer
-- Returns True iff the number is a palindrome, e.g., isPalindrome 12321 returns True, and
-- isPalindrome 12321 is False. A negative number is never a palindrome.
isPalindrome :: Integer -> Bool

-- Returns True if the number is a prime number. A negative number is never a prime number.
isPrime :: Int -> Bool
-- Returns the next prime number. If the number itself is already prime, return it.
-- So nextPrime 10 is 11, nextPrime 11 is also 11.
-- Hint: Use until
nextPrime :: Int -> Int
-- Sums the first N primes. So sumPrimes 4 is 17.
sumNPrimes :: Int -> Int

-- Returns true if the number is the first of a pair of twin primes. A twin prime is a number whose next prime
-- is itself +2. So 3 is a twinprime. So is 5. 7 is not. 11 is once again a twin prime, and 13 is not.
isTwinPrime :: Int -> Bool
-- Returns the next twin prime. If the number itself is already a twin prime, return it.
-- So nextTwinPrime of 18 is 29. nextTwinPrime of 29 is 29.
-- Hint: Use until
nextTwinPrime :: Int -> Int
-- Returns the nth twin prime. Assume the number is positive. So the nthTwiPrime of 100 is 3821.
nthTwinPrime :: Int -> Int

-- Returns true if the number is a perfect number. A perfect number is equal to the sum of its
-- divisors. So 6 (1 + 2 + 3) a is a perfect number, so is 28 (1 + 2 + 4 + 7 + 14), and so is 496.
isPerfectNumber :: Int -> Bool