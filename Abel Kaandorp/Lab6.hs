module Lab6 where
    
import Lecture6
import SetOrd
import Data.Tuple
import Data.List
import System.Random
import Control.Monad
import Test.QuickCheck

--Exercise 1: 

exM :: Integer -> Integer -> Integer -> Integer
exM n 0 m = 1




--powm :: Integer -> Integer -> Integer -> Integer -> Integer
--powm b 0 m r = 1
--powm b e m r
 -- | e `mod` 2 == 1 = powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
--powm b e m r = powm (b * b `mod` m) (e `div` 2) m rp




--  Exercise 3:     Write a function composites :: [Integer] that generates the 
--                  infinite list of composite natural numbers.
--  Time: 0.5 hours

composites :: [Integer]
composites = [x | x <- [2..], not (prime x)]

--  Exercise 4:     Use the list of composite numbers to test Fermat's 
--                  primality check.
--  Time: 2 hours

leastComposite :: Int -> [Integer] -> IO ()
leastComposite p (x:xs) = do res <- primeTestsF k x
                             if res then print ("value" ++ show k ++ " failed on: " ++ show x)
                             else testPrimes k xs

-- An increase on 'k' means that the results of function 'primeTestsF' will in turn
-- be more accurate. So that means that the bigger the number the greater the value.

-- Exercise 5

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
        k <- [2..], 
        prime (6*k+1), 
        prime (12*k+1), 
    prime (18*k+1) ]

-- Exercise 7: You can use the Miller-Rabin primality check to discover some large Mersenne primes. 
-- The recipe: take a prime p, and use the Miller-Rabin algorithm to check whether 2p−12p−1 is also prime.
-- Find information about Mersenne primes on internet and check whether the numbers that you found are genuine Mersenne primes.
--  Report on your findings.

mersenneNumber :: Integer -> Integer
mersenneNumber n = 2 ^ n - 1



isMersennePrime :: Integer -> IO Bool
isMersennePrime p = primeMR (2^p - 1) p




--filterMersennes :: Integer -> IO Bool
--filterMersennes p = primeMR p 10



--mersenneTest :: Int -> Bool
--mersenneTest p | primeMR 10 p && primeMR 10 (2^p -1) = True
--               | otherwise = False



mersennes :: [Int] -> [Int]
mersennes m = [p | p <- m,
   iterate (\n -> mod (n^2 - 2) (2^p - 1)) 4 !! p-2 == 0]    

   

--testForMersennes = printMersennes $ take 45 $ filter millerRabinPrimality $ sieve [2..]
--printMersennes = mapM_ (\x -> putStrLn $ "M" ++ show x)

-- Exercise 7: Bonus