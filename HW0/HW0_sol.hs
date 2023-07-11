-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW0.hs should successfully compile.
--
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW0 where

-- These import statement ensures you aren't using any "advanced" functions and types, e.g., lists.
import Prelude (Bool (..), Eq (..), Int, Integer, Num (..), Ord (..), div, error, flip, id, mod, ($), (&&), (.), (||))

-- For a bit of an extra challenge, use the below import list instead, and implement the missing functions yourselves!
-- import Prelude (Num(..), Bool(..), Int, Integer, Eq(..), Ord(..), div, error)

-- Higher order functions. These should be self-explanatory.
const :: a -> b -> a
const a _ = a

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

(.:) :: (a3 -> a4) -> (a1 -> a2 -> a3) -> a1 -> a2 -> a4
-- An alternative implementation to 'f g x y = f $ g x y'.
(.:) g h = (.) g . h

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f = f .:. (,,) where
  -- If f . g is composition where g accept one argument,
  -- and f .: g is composition where g accepts two arguments,
  -- then f .:. g is composition where g accepts three arguments!
  -- There are multiple ways of implementing this function, of course, but this one a very nice
  -- symmetry to it!
  (.:.) :: (a4 -> a5) -> (a1 -> a2 -> a3 -> a4) -> a1 -> a2 -> a3 -> a5
  (.:.) g h = (.:) g . h


uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

rotate :: (a -> b -> c -> d) -> c -> a -> b -> d
rotate f c a b = f a b c
lotate :: (a -> b -> c -> d) -> b -> c -> a -> d
lotate f b c a = f a b c

-- or (> 0) (< 0) 5 returns True
-- or (> 0) (< 0) 0 returns False
or :: (a -> Bool) -> (a -> Bool) -> a -> Bool
or = bool (||)
-- and (> 0) (< 0) 5 returns False
-- and (> 0) (> 10) 20 returns True
and :: (a -> Bool) -> (a -> Bool) -> a -> Bool
and = bool (&&)

-- This one can actually be more general than this. Try removing the signature and asking HLS!
bool :: (Bool -> Bool -> Bool) -> (a -> Bool) -> (a -> Bool) -> a -> Bool
bool op f g a = op (f a) (g a)

-- Applies the function until some condition holds.
-- until (+1) (> 0) 5 returns 5
-- until (*2) (> 10) 1 returns 16
-- until (*1) (> 0) 0 would never terminate!
until :: (a -> a) -> (a -> Bool) -> a -> a
until next p seed = if p seed then seed else until next p (next seed)

-- Numerical functions and algorithms.

-- Returns the greatest common divisor (GCD) of two numbers. The GCD is always a positive number.
-- gcd 123456 432 is 48.
gcd :: Int -> Int -> Int
gcd a 0 = abs a
gcd a b = gcd b (a `mod` b)

-- Returns the least common multiplier (LCM) of two numbers. The LCM is always a positive number.
-- lcm 123456 432 is 1111104
-- Hint: you can use gcd, or implement it recursively.
lcm :: Int -> Int -> Int
lcm a b =
  let step = max a b
      loop :: Int -> Int
      loop = until (+ step) (\n -> a `divides` n && b `divides` n)
   in abs $ loop step
lcm' :: Int -> Int -> Int
lcm' a b = abs (a * b) `div` gcd a b

-- Returns the length of the Collatz sequence. The Collatz function is defined thus:
-- collatz of an even number is the number divided by 2.
-- collatz of an uneven number greater than 1 is collatz of 3 * n + 1.
-- A collatz sequence, is the sequence of element until the number reaches 1.
-- For example, for 13, the sequence is 13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1.
-- The length of the sequence is 10.
-- Write a function which computes the length of the sequence for a given positive integer n.
-- collatzSequence 13 is 10.
-- collatzSequence of 0 or -13 is 0.
collatzSequence :: Int -> Int
collatzSequence n | n <= 0 = 0
collatzSequence 1 = 1
collatzSequence n = 1 + if 2 `divides` n then n `div` 2 else 3 * n + 1

-- Count the number of digits in an integer. For example, countDigits 123 should return 3.
-- A negative number has the same number of digits as its absolute value, so countDigits (-123)
-- is also 3.
countDigits :: Integer -> Int
countDigits n | n < 0 = 1 + countDigits (- n)
countDigits n | n < 10 = 1
countDigits n = 1 + countDigits (n `div` 10)

-- Returns the number reversed. For example, reverseNum of 123 is 321. The reverse of a negative
-- number is also negative, so negativeNum (-123) is -321.
reverseNum :: Integer -> Integer
reverseNum n | n < 0 = - (reverseNum (-1 * n))
reverseNum n | n < 10 = n
reverseNum n =
  let lastDigit = n `mod` 10
      withoutLastDigits = n `div` 10
   in lastDigit * tenToThePowerOf (countDigits withoutLastDigits) + reverseNum withoutLastDigits
 where
  tenToThePowerOf :: Int -> Integer
  tenToThePowerOf 0 = 1
  tenToThePowerOf m | m > 0 = 10 * tenToThePowerOf (m - 1)
  tenToThePowerOf _ = error "WTF"
-- Returns True iff the number is a palindrome, e.g., isPalindrome 12321 returns True, and
-- isPalindrome 12321 is False. A negative number is never a palindrome.
isPalindrome :: Integer -> Bool
isPalindrome n = n == reverseNum n

-- Returns True if the number is a prime number. A negative number is never a prime number.
isPrime :: Int -> Bool
isPrime n | n < 2 = False
isPrime 2 = True
isPrime n = test 2
 where
  test :: Int -> Bool
  test i = i == n || not (i `divides` n) && test (i + 1)
  -- Probably should have been imported from Prelude... Whoops.
  not :: Bool -> Bool
  not True = False
  not False = True
-- Returns the next prime number. If the number itself is already prime, return it.
-- So nextPrime 10 is 11, nextPrime 11 is also 11.
-- Hint: Use until
nextPrime :: Int -> Int
nextPrime = untilPlus1 isPrime
-- Sums the first N primes. So sumPrimes 4 is 17.
sumNPrimes :: Int -> Int
sumNPrimes n = go n 0 0
 where
  go :: Int -> Int -> Int -> Int
  go 0 sum _ = sum
  go k sum p = let np = nextPrime p in go (k - 1) (sum + np) (np + 1)


-- Returns true if the number is the first of a pair of twin primes. A twin prime is a number whose next prime
-- is itself +2. So 3 is a twinprime. So is 5. 7 is not. 11 is once again a twin prime, and 13 is not.
isTwinPrime :: Int -> Bool
isTwinPrime n = isPrime n && isPrime (n + 2)
-- Returns the next twin prime. If the number itself is already a twin prime, return it.
-- So nextTwinPrime of 18 is 29. nextTwinPrime of 29 is 29.
-- Hint: Use until
nextTwinPrime :: Int -> Int
nextTwinPrime = untilPlus1 isTwinPrime
-- Returns the nth twin prime. Assume the number is positive. So the nthTwiPrime of 100 is 3821.
nthTwinPrime :: Int -> Int
nthTwinPrime k = untilN k (+ 1) isTwinPrime 0 where
  untilN :: Int -> (a -> a) -> (a -> Bool) -> a -> a
  untilN 0 _ _ = id
  untilN n next p = untilN (n - 1) next p . until next p . next

-- Returns true if the number is a perfect number. A perfect number is equal to the sum of its
-- divisors. So 6 (1 + 2 + 3) is a perfect number, so is 28 (1 + 2 + 4 + 7 + 14), and so is 496.
isPerfectNumber :: Int -> Bool
isPerfectNumber n = sumDivisors 0 1 == n
 where
  sumDivisors :: Int -> Int -> Int
  sumDivisors sum i | i == n = sum
  sumDivisors sum i = sumDivisors (if i `divides` n then sum + i else sum) (i + 1)

-- Useful utilities
untilPlus1 :: (Int -> Bool) -> Int -> Int
untilPlus1 = until (+ 1)

divides :: Int -> Int -> Bool
divides = (== 0) .: flip mod