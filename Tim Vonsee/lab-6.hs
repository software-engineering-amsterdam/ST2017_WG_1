
module Lecture6

where

import System.Random

factorsNaive :: Integer -> [Integer]
factorsNaive n0 = factors' n0 2 where
  factors' 1 _ = []
  factors' n m
    | n `mod` m == 0 = m : factors' (n `div` m) m
    | otherwise      =     factors' n (m+1)

factors :: Integer -> [Integer]
factors n0 = let
   ps0 = takeWhile (\ m -> m^2 <= n0) primes
 in factors' n0 ps0 where
   factors' 1 _  = []
   factors' n [] = [n]
   factors' n (p:ps)
    | n `mod` p == 0 = p: factors' (n `div` p) (p:ps)
    | otherwise      =    factors' n ps

prime :: Integer -> Bool
prime n = factors n == [n]

primes :: [Integer]
primes = 2 : filter prime [3..]

mers :: Integer -> Integer
mers 1  = 2^2-1;    mers 2  = 2^3-1;     mers 3  = 2^5-1
mers 4  = 2^7-1;    mers 5  = 2^13-1;    mers 6  = 2^17-1
mers 7  = 2^19-1;   mers 8  = 2^31-1;    mers 9  = 2^61-1
mers 10 = 2^89-1;   mers 11 = 2^107-1;   mers 12 = 2^127-1
mers 13 = 2^521-1;  mers 14 = 2^607-1;   mers 15 = 2^1279-1
mers 16 = 2^2203-1; mers 17 = 2^2281-1;  mers 18 = 2^3217-1
mers 19 = 2^4253-1; mers 20 = 2^4423-1;  mers 21 = 2^9689-1
mers 22 = 2^9941-1; mers 23 = 2^11213-1; mers 24 = 2^19937-1
mers 25 = 2^21701-1;
mers _  = undefined

bin2int :: [Int] -> Int
bin2int = bin . reverse where
  bin []  = 0
  bin [0] = 0
  bin [1] = 1
  bin (0:bs) = 2 * bin bs
  bin (1:bs) = 2 * bin bs + 1
  bin _      = error "not a binary digit list"

addM :: Integer -> Integer -> Integer -> Integer
addM x y = rem (x+y)

multM :: Integer -> Integer -> Integer -> Integer
multM x y = rem (x*y)

invM :: Integer -> Integer -> Integer
invM x n = let
   (u,v) = fctGcd x n
   copr  = x*u + v*n == 1
   i     = if signum u == 1 then u else u + n
 in
   if copr then i else error "no inverse"

fGcd :: Integer -> Integer -> Integer
fGcd a b = if b == 0 then a
                     else fGcd b (rem a b)

fctGcd :: Integer -> Integer -> (Integer,Integer)
fctGcd a b =
  if b == 0
  then (1,0)
  else
     let
       (q,r) = quotRem a b
       (s,t) = fctGcd b r
     in (t, s - q*t)

coprime :: Integer -> Integer -> Bool
coprime n m = fGcd n m == 1

coprime' :: Integer -> Integer -> Bool
coprime' n m = let (x,y) = fctGcd n m
               in x*n + y*m == 1

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

grow :: (node -> [node]) -> node -> Tree node

grow step seed = T seed (map (grow step) (step seed))

takeT :: Int -> Tree a -> Tree a

takeT 0 (T x _) = T x []
takeT n (T x ts) = T x (map (takeT (n-1)) ts)

coprimeT :: Tree (Integer,Integer)
coprimeT = grow f (1,1)

f :: (Integer,Integer) -> [(Integer,Integer)]
f (n,m) = [(n+m,m),(n,n+m)]

pairs :: [(Integer,Integer)]
pairs = concatMap (\ n -> zip [1..n] (repeat n)) [1..]

coprimes :: [(Integer,Integer)]
coprimes = filter (uncurry coprime) pairs

expM ::  Integer -> Integer -> Integer -> Integer
expM x y = rem (x^y)

primeTestF :: Integer -> IO Bool
primeTestF n = do
   a <- randomRIO (2, n-1) :: IO Integer
   return (exM a (n-1) n == 1)

primeTestsF :: Int -> Integer -> IO Bool
primeTestsF k n = do
 as <- sequence $ fmap (\_-> randomRIO (2,n-1)) [1..k]
 return (all (\ a -> exM a (n-1) n == 1) as)

decomp :: Integer -> (Integer,Integer)
decomp n0 = decomp' (0,n0) where
  decomp' = until (odd.snd) (\ (m,n) -> (m+1,div n 2))

mrComposite :: Integer -> Integer -> Bool
mrComposite x n = let
    (r,s) = decomp (n-1)
    fs     = takeWhile (/= 1)
       (map (\ j -> exM x (2^j*s) n)  [0..r])
  in
    exM x s n /= 1 && last fs /= (n-1)

primeMR :: Int -> Integer -> IO Bool
primeMR _ 2 = return True
primeMR 0 _ = return True
primeMR k n = do
    a <- randomRIO (2, n-1) :: IO Integer
    if exM a (n-1) n /= 1 || mrComposite a n
    then return False else primeMR (k-1) n

encodeDH :: Integer -> Integer -> Integer -> Integer
encodeDH p k m = m*k `mod` p

decodeDH :: Integer -> Integer -> Integer -> Integer -> Integer
decodeDH p ga b c = let
    gab' = exM ga ((p-1)-b) p
  in
    rem (c*gab') p

encode :: Integer -> Integer -> Integer -> Integer
encode p k m = let
   p' = p-1
   e  = head [ x | x <- [k..], gcd x p' == 1 ]
 in
   exM m e p

decode :: Integer -> Integer -> Integer -> Integer
decode p k m = let
   p' = p-1
   e  = head [ x | x <- [k..], gcd x p' == 1 ]
   d  = invM e p'
 in
   exM m d p

cipher :: Integer -> Integer
cipher = encode secret bound

decipher :: Integer -> Integer
decipher = decode secret bound

totient :: Integer -> Integer
totient n = toInteger $ length [ k | k <- [1..n], gcd k n == 1 ]

phi :: Integer -> Integer -> Integer
phi p q = (p - 1) * (q - 1)

select :: Integer -> Integer -> Integer
select p q = let
   t = phi p q
 in
   head [ x | x <- [3..], gcd x t == 1 ]

rsaPublic :: Integer -> Integer -> (Integer,Integer)
rsaPublic p q = let
    e = select p q
  in
    (e,p*q)

rsaPrivate ::  Integer -> Integer -> (Integer,Integer)
rsaPrivate p q = let
   e = select p q
   t = phi p q
   d = invM e t
  in
   (d,p*q)

rsaEncode :: (Integer,Integer) -> Integer -> Integer
rsaEncode (e,n) m =  exM m e n

rsaDecode :: (Integer,Integer) -> Integer -> Integer
rsaDecode = rsaEncode

trapdoor :: (Integer,Integer) -> Integer -> Integer
trapdoor = rsaEncode

secret, bound :: Integer
secret = mers 18
bound  = 131

--------------------------------------------------------------------------------
{-
                              DEFINITIONS

Carmichael number:      is a composite number n which satisfies the modular arithmetic
                        congruence relation: b^(n-1) = 1 * (mod n)
Composite:              number with more than 2 factors
Prime:                  2 factors -> zichzelf en 1
Factors:                de getallen waar een getal door gedeeld kan worden zonder remainder
Composite:              number with more than 2 factors
Relatively-/co-prime):  greatest common divisor is 1.

-}

exM :: Integer -> Integer -> Integer -> Integer
exM n f m | f == 0 = 1 `mod` m
          | f == 1 = n `mod` m
          | even f = (divM * divM) `mod` m
          | otherwise = (n * prevMod) `mod` m
              where
                divM = exM n (f `div` 2) m
                prevMod = exM n (f-1) m


--------------------------EXERCISE 3  (Time 30m)--------------------------------
-- Write a function composites :: [Integer] that generates the infinite list
-- of composite natural numbers.

-- Find all factors of a given number
factors' :: Integer -> [Integer]
factors' n = filter (\x -> n `mod` x == 0) [1..n]

-- Check if a number is composite (has more than two factors)
isComposite :: Integer -> Bool
isComposite x = length (factors' x) > 2

-- Generate a infinite list of factors
composites :: [Integer]
composites = [x | x <- [1..], isComposite x]

-- Run: take 100 composites

--------------------------EXERCISE 4  (Time 2u)--------------------------------
-- Use the list of composite numbers to test Fermat's primality check. What is
-- the least composite number that you can find that fools the check, for
-- prime_tests_F k with k=1,2,3 ? What happens if you increase k?

-- Get least composites for a given number
getLeastComposite :: Int -> [Integer] -> IO [Integer]
getLeastComposite _ [] = return []
getLeastComposite k (x:xs) = do val <- primeTestsF k x
                                if val then do rest <- getLeastComposite k xs
                                               return (x : rest)
                                else getLeastComposite k xs

-- Run: getLeastComposite 1 (take 1000 composites)

----------------------------EXERCISE 5(Time 15m)--------------------------------
{-
  Use the list generated by the following function for a further test of
  Fermat's primality check.
  Read the entry on Carmichael numbers on Wikipedia to explain what you find.
  If necessary, consult other sources.
-}

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
      k <- [2..],
      prime (6*k+1),
      prime (12*k+1),
      prime (18*k+1) ]

-- Run: getLeastComposite 1 (take 3 carmichael)

----------------------------EXERCISE 6(Time 15m)--------------------------------

-- primeMR :: Int -> Integer -> IO Bool
-- primeMR _ 2 = return True
-- primeMR 0 _ = return True
-- primeMR k n = do
--     a <- randomRIO (2, n-1) :: IO Integer
--     if exM a (n-1) n /= 1 || mrComposite a n
--     then return False else primeMR (k-1) n

getMR :: Int -> [Integer] -> IO [Integer]
getMR _ [] = return []
getMR k (x:xs) = do val <- primeMR k x
                    if val then do rest <- getLeastComposite k xs
                                   return (x : rest)
                    else getLeastComposite k xs

-- Run getMR 1 (take 10 carmichael)


--
