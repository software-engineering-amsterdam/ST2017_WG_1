module Lab1 where
import Data.List
import Test.QuickCheck 

prime :: Int -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Int]
primes = 2 : filter prime [3..] 

{-- Exercise 1
    Prove by induction that it holds for all natural numbers
    n that ... 
    Time: 3,5 hours
    --}

-- Exercise 1.1

ex11lHs :: Int -> Int
ex11lHs n = sum[x ^ 2 | x <- [1..n]]

ex11rHs :: Int -> Int
ex11rHs n = (n * (n+1) * (2 * n + 1)) `div` 6

ex11tst :: Int -> Bool
ex11tst n | n >= 0 = ex11lHs n == ex11rHs n
          | otherwise = True
-- implement quickcheck

-- Exercise 1.2

ex12lHs :: Int -> Int
ex12lHs n = sum[x ^ 3 | x <- [1..n]]

ex12rhs :: Int -> Int
ex12rhs n = (n * (n + 1) `div` 2)^ 2

ex12tst :: Int -> Bool
ex12tst n | n >= 0 = ex11lHs n == ex11rHs n
          | otherwise = True
-- implement quickheck


-- Exercise 2
-- Prove by induction that it holds for all natural numbers
-- n that ...

ex2 :: Int -> Bool
ex2 = 2 ^ n == length (subsequences [1..n])

--TODO: Finish

{-- Exercise 3
-- Prove by induction that if A is a finite set with
-- |A| = n then... --}

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
     insrt x [] = [[x]]
     insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

factorial:: Int -> Int
factorial n = product([1..n]) 

--tstPerms :: Int -> Bool
--tstPerms x = length perms [1..x] == factorial x 

-- TODO: Finish test


-- Exercise 4

-- Reverse an Int
reversal :: Int -> Int
reversal = read . reverse . show

-- src: https://wiki.haskell.org/Testing_primality
isPrime :: Int -> Bool
isPrime n = n > 1 &&
            foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r))
                True primes

-- Reverse a prime
reversedPrime :: Int -> Bool
reversedPrime n = isPrime (reversal n)

generateReversedPrimes = [ x | x <- [1..10000], reversedPrime x]

-- Exercise 5

-- All primes to N
primeToN :: Int -> [Int]
primeToN x = [ n | n <- 1 : 2 : [3,5..x-1], isPrime n ]

---



-- Exercise 6



-- Exercise 7
luhn :: Integer -> Bool

isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress n = 