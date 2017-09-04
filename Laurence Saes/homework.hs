module Lab1 where
import Data.List
import Test.QuickCheck

import Test.QuickCheck

-- Functions for the slides
-- Credits for isqrt https://stackoverflow.com/questions/6695267/haskell-get-sqrt-from-int
isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

-- Credits for isPrime https://stackoverflow.com/questions/4690762/determining-if-a-given-number-is-a-prime-in-haskell
isPrime k = null [ x | x <- [2..isqrt k], k `mod` x  == 0]

primes :: [Int]
primes = 2 : filter isPrime [3..]

-- Exercises

-- The left side of the expression
wextr2l :: Int -> Int
wextr2l n = sum [ x^2 | x <- [1..n] ]

-- The right side of the expression
wextr2r :: Int -> Int
wextr2r n = (n * (n+1) * (2 * n + 1)) `div` 6

-- Test the functions, note that negative numbers are not tested
extrs1 :: Int -> Bool
extrs1 n | n >= 0 = wextr2l n == wextr2r n
         | otherwise = True

-- Run quickCheck extrs1 for extersize 1

-- The left side of the expression
wextr3l :: Int -> Int
wextr3l n = sum [ x^3 | x <- [1..n] ]

-- The right side of the expression
wextr3r :: Int -> Int
wextr3r n = ((n * (n+1)) `div` 2) ^ 2

-- Test the functions, note that negative numbers are not tested
extrs1s :: Int -> Bool
extrs1s n | n >= 0 = wextr3l n == wextr3r n
         | otherwise = True

-- Run quickCheck extrs1s for extersize 1

-- Prove by induction that if AA is a finite set with |A|=n|A|=n, then |P(A)|=2n|P(A)|=2n.

extrs2 :: Int -> Bool
extrs2 n = 2 ^ setLength == powerSetLength
                where set = [1..n]
                      setLength = length set
                      powerSet = subsequences set
                      powerSetLength = length powerSet

-- run quickCheck extrs2

-- The property is hard to test because of large input data.
-- Whenever you want to test for exaple the number 1.000.000
-- you are creating a list of 2^1.000.000 elements.

-- With this test you are testing a mathematical fact. When you create a list
-- with N elements. Then every element can be in the list ( yes or no ). So
-- it always are 2^n elements.

-- With this test you know that the haskell functions work that you are using.

-- Credits for factorial function: http://vvv.tobiassjosten.net/haskell/factorials-in-haskell/
factorial 0 = 1
factorial n = n * factorial (n - 1)

extrs3 :: Int -> Bool
extrs3 n | n <= 0 = True
         | otherwise = factorial n == perms
           where set = [1..n]
                 perms = length (permutations set)

-- run quickCheck extrs3

reversal :: Int -> Int
reversal = read . reverse . show

reversalPrime :: Int -> Bool
reversalPrime n = isPrime (reversal n)

reversePrimes :: [Int] -> [Int]
reversePrimes xs = filter reversalPrime xs

reversePrimesMax n = takeWhile (< n) primes

-- Run reversePrimesMax 10000 for a list of reversal primes
-- This function can be compared with a list of all reversed primes to 10000.

-- Take ps primes and test if prime
getConsecutivePrimes :: Int -> [Int] -> [Int]
getConsecutivePrimes ps primesL | length primeList < ps = []
                                     | listIsPrime = primeList
                                     | otherwise = next
                                      where primeList = take ps primesL
                                            listIsPrime = isPrime (sum primeList)
                                            next = getConsecutivePrimes ps (tail primesL)

-- length primeList has to be used because primesL is infinit. Length of primesL would cause in an
-- infinite loop
-- Run with getConsecutivePrimes 101 primes then the answer will be:
{-
  [83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,661,673,677]
-}

-- Credits for digs https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs) = f x : altMap g f xs

luhnDouble' :: Int -> Int
luhnDouble' x | x * 2 > 9 = x * 2 - 9
             | otherwise = x * 2

luhn' :: [Int] -> Bool
luhn' xs = sum (altMap (+0) (luhnDouble') (reverse xs)) `mod` 10 == 0

luhn :: Int -> Bool
luhn x = luhn' (digs x)
