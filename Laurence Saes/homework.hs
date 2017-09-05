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

extrs1Answer1 = quickCheck (\(Positive x) -> wextr2l x == wextr2r x)

-- The left side of the expression
wextr3l :: Int -> Int
wextr3l n = sum [ x^3 | x <- [1..n] ]

-- The right side of the expression
wextr3r :: Int -> Int
wextr3r n = ((n * (n+1)) `div` 2) ^ 2

extrs1Answer2 = quickCheck (\(Positive x) -> wextr3l x == wextr3r x)

-- Run quickCheck (\(Positive x) -> wextr3l x == wextr3r x)

-- Prove by induction that if AA is a finite set with |A|=n|A|=n, then |P(A)|=2n|P(A)|=2n.

extrs2 :: Int -> Bool
extrs2 n = 2 ^ setLength == powerSetLength
                where set = [1..n]
                      setLength = length set
                      powerSet = subsequences set
                      powerSetLength = length powerSet

-- run quickCheck extrs2
extrs2Answer = quickCheck (\(Positive x) -> extrs2 x)

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
extrs3 n = factorial n == perms
           where set = [1..n]
                 perms = length (permutations set)

extrs3Answer = quickCheck (\(Positive x) -> extrs3 x)

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
-- Run with sum (getConsecutivePrimes 101 primes) then the answer will be:
{-
  37447:
  [83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,661,673,677]
-}

-- n is consecutive list length
-- plist is a list of primes
consecutivePrimeTestC :: Int -> [Int] -> [Int]
consecutivePrimeTestC n plist | not (isPrime primeNumber) = primeList
                              | otherwise = consecutivePrimeTestC n (tail plist)
                                where primeList = take n plist
                                      primeNumber = (sum (primeList)) + 1

-- Test with consecutivePrimeTestC n primes where n how large the list of primes have to be
-- What is the smallest counterexample?
-- with a list of 2 it is [2,3]
-- with a list of 3 it is [3,5,7]
-- with a list of 4 it is [2,3,5,7]


-- Credits for digs https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

-- Apply function f on 1,3,5,7,9... and g on 2,4,6,8....
flipMap :: (a -> a) -> (a -> a) -> [a] -> [a]
flipMap _ _ [] = []
flipMap f g l = f (head l) : flipMap g f (tail l)

-- Compress number for luhn
luhnCompress :: Int -> Int
luhnCompress z = sum (digs z)

-- Do the check
luhn :: Int -> Bool
luhn x = sum checkValues `mod` 10 == 0
          where intList = digs x
                revList = reverse intList
                checkValues = flipMap id (\x -> luhnCompress (x*2)) revList

isAmericanExpress :: Int -> Bool
isAmericanExpress x = longEnought && (startCheck1 || startCheck2) && luhn x
                      where showX = show x
                            longEnought = length showX == 15
                            startCheck1 = isPrefixOf "34" showX
                            startCheck2 = isPrefixOf "37" showX

isMaster :: Int -> Bool
isMaster x = longEnought && (startCheck1 || startCheck2) && luhn x
                      where showX = show x
                            longEnought = length showX == 16
                            startCheck1 = or (map (\n -> isPrefixOf (show n) showX) [2221..2720] )
                            startCheck2 = or (map (\n -> isPrefixOf (show n) showX) [51..55] )

isVisa :: Int -> Bool
isVisa x = longEnought && startCheck && luhn x
                      where showX = show x
                            longEnought = (length showX) `elem` [13,16,19]
                            startCheck = isPrefixOf "4" showX

-- luhn check. The last digit is the check bit. One out of 10 is a correct number
generateCreditCardNumbers :: Int -> Int -> [Int]
generateCreditCardNumbers i l | inputLength == l - 1 = map (i*10+) [0..9]
                              | inputLength >= l = generateCreditCardNumbers (i `div` 10) l
                              | inputLength < l = generateCreditCardNumbers (i*10) l
                                where inputLength = length (show i)

-- Test if one in each group is valid
testGroup :: (Int -> Bool) -> [[Int]] -> [Bool]
testGroup f i = map (\g -> length (filter (==True) ( map f g)) == 1 ) i

prependNumber :: Int -> Int -> Int
prependNumber i j = i * (10 ^ (length (show j))) + j

generateCardNumbers :: Int -> [Int] -> [Int] -> [[Int]]
generateCardNumbers i ccLengths startWith = concat (
                                                  map (\x ->
                                                      map ( \ccLength ->
                                                        generateCreditCardNumbers (prependNumber x i) ccLength )
                                                      ccLengths )
                                                  startWith )

-- start with 34 or 37 and the size is 15
testAmericanExpress :: Int -> Bool
testAmericanExpress i = and doTest
                        where ccLength = [15]
                              startWith = [34,37]
                              groups = generateCardNumbers i ccLength startWith
                              doTest = testGroup isAmericanExpress groups

testMasterCard :: Int -> Bool
testMasterCard i = and doTest
                      where ccLength = [16]
                            startWith = concat [[2221..2720],[51..55]]
                            groups = generateCardNumbers i ccLength startWith
                            doTest = testGroup isMaster groups

testVisa :: Int -> Bool
testVisa i = and doTest
                    where ccLength = [13,16,19]
                          startWith =[4]
                          groups = generateCardNumbers i ccLength startWith
                          doTest = testGroup isVisa groups

-- Run with quickCheckWith stdArgs { maxSize = 9999999999999 }  (\(Positive x) -> testAmericanExpress x)
-- Run with quickCheckWith stdArgs { maxSize = 99999999999999 }  (\(Positive x) -> testMasterCard x)
-- Run with quickCheckWith stdArgs { maxSize = 999999999999999999 }  (\(Positive x) -> testVisa x)

-- isAmericanExpress, isMaster, isVisa :: Integer -> Bool


-- 	(a -> b -> b) -> b -> [a] -> b
