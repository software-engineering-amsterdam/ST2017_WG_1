module Main

where

import System.Random
import Test.QuickCheck

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
 return (all (\ a -> exM a (n-1)n == 1) as)

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


--
-- Exercise 1
-- Time 4 hours

exM :: Integer -> Integer -> Integer -> Integer
exM n f m | f == 0 = 1 `mod` m
          | f == 1 = n `mod` m
          | even f = (divM * divM) `mod` m
          | otherwise = (n * prevMod) `mod` m
              where divM = exM n (f `div` 2) m
                    prevMod = exM n (f-1) m

-- Quick check to compare to the normal mod
pexM (Positive n) (Positive f) (Positive m) = exM n f m == (n^f) `mod` m
pexM' n f m = exM n f m == (n^f) `mod` m

main :: IO ()
main = do quickCheck pexM

{-
main
+++ OK, passed 100 tests.
-}


-- Exercise 2
-- Time 1 hour

{-
  expM 34 809700 23
  12
  (0.04 secs, 1,458,128 bytes)

  exM 34 809700 23
  12
  (0.00 secs, 83,824 bytes)

  expM 34 8097000 23
  2
  (0.41 secs, 14,906,312 bytes)

  exM 34 8097000 23
  2
  (0.01 secs, 86,456 bytes)

  expM 34 80970000 23
  12
  (5.26 secs, 150,256,256 bytes)

  exM 34 80970000 23
  12
  (0.00 secs, 86,400 bytes)

  Executing speed
  factor                exM               expM
  f = 809700            0.00              0.04
  f = 8097000           0.01              0.41
  f = 80970000          0.00              5.26

  Memory usage
  factor                exM                 expM
  f = 809700            83,824 bytes        1,458,128 bytes
  f = 8097000           86,456 bytes        14,906,312 bytes
  f = 80970000          86,400 bytes        150,256,256 bytes

  You can see that the speed and memory usage climbs very fast with expM and not with exM
-}

-- Exercise 3
-- Time 1 hour

composites :: [Integer]
composites = [x | x <- [2..], (not . prime) x ]

-- Exercise 4
-- Time 1 hour

-- https://stackoverflow.com/questions/27892035/list-average-for-ints
average :: [Integer] -> Integer
average xs = (fromInteger (sum xs)) `div` (toInteger (length xs))

findFalsePositive :: Int -> [Integer] -> IO (Integer)
findFalsePositive k testValues = do pTest <- primeTestsF k (check)
                                    if pTest then return check
                                             else (findFalsePositive k (tail testValues))
                                    where check = head testValues

findFalsePositiveList :: Int -> [Integer] -> Int -> IO [Integer]
findFalsePositiveList k testValues n | n <= 0 = return []
                                     | otherwise = do x <- findFalsePositive k testValues
                                                      n <- findFalsePositiveList k testValues (n-1)
                                                      return (x : n)

getFalsePositiveAvg :: Int -> [Integer] -> Int -> IO (Integer)
getFalsePositiveAvg k testValues n = do list <- findFalsePositiveList k testValues n
                                        return (average list)


findFalsePositive' :: Int -> [Integer] -> IO ([Integer])
findFalsePositive' _ [] = return []
findFalsePositive' k (check:checks) = do pTest <- primeTestsF k (check)
                                         if pTest then do rest <- (findFalsePositive' k checks)
                                                          return (check : rest)
                                         else (findFalsePositive' k checks)

{-
findFalsePositive 1 composites
15

findFalsePositive 2 composites
301

findFalsePositive 3 composites
1105

When you do multiple tests, you see that the value of findFalsePositive will differ. I did an extra test where we calculate the average
of 1000 tests:

getFalsePositiveAvg 1 composites 1000
37

getFalsePositiveAvg 2 composites 1000
406

getFalsePositiveAvg 3 composites 1000
1481

Now you can see that the false positives will accrue later when the k is increased.

I also made a function that can see what the false positives are:

findFalsePositive' 1 (take 10000 composites)
[15,28,33,91,231,259,341,435,481,561,753,793,1001,1105,1185,1269,1441,1729,1825,2041,2071,2465,2821,3031,3085,3201,3277,3486,3681,3751,3937,3988,4123,4485,4641,4681,4961,5083,5551,5611,5662,6533,6601,6643,7381,7921,8614,8695,8841,8911,9537,9729,9773,10027,10065,10423,10945,11011]

findFalsePositive' 2 (take 10000 composites)
[169,305,1105,3097,6601,8321]

findFalsePositive' 3 (take 10000 composites)
[1105,1541,2701,8401,8911]

-}

-- Exercise 5
-- Time 1 hour

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
     k <- [2..],
     prime (6*k+1),
     prime (12*k+1),
     prime (18*k+1) ]


{-
findFalsePositive' 1 (take 100 carmichael)
[294409,56052361,118901521,172947529,216821881,228842209,1299963601,2301745249,9624742921,11346205609,13079177569,21515221081,27278026129,65700513721,71171308081,100264053529,168003672409,172018713961,173032371289,464052305161,527519713969,663805468801,727993807201,856666552249,1042789205881,1201586232601,1396066334401,1544001719761,1797002211241,1920595706641,2028691238689,2655343122121,2718557844481,2724933935809,2920883888089,3091175755489,3267961077889,3296857440241,3414146271409,3711619793521,3719466204049,3878725359169,4287981117241,4507445537641,6323547512449,7622722964881,8544361005001,8681793690961,9332984447209,11004252611041,11413778221441,11765530852489,13633039686169,14470947115561,14685655594249,14882678745409,15181505298649,17167430884969,18483957064801,20742413217121,21873528379441,22027380041449,24285059687809,24977268314209,25825129162489,30833142247729,33614369156161,35700127755121,37686301288201,39782913594409,48336382727569,53269464581929,57060521336809,58774132848169,62303597046289,67858397221969,70895483772049,73103085605161,77833567590769,81159260227849,81466208375329,86483161466209,91968282854641,95682503446921,98445661027561,105950928237841,112374872517529,118895125737961,118974229155289,122570307044209,127393969917241,129140929242289,137243534644009,168011973623089,177548395641481,184455452572849,192410140250521,195809339861929,201375886537729,221568419989801]

findFalsePositive' 2 (take 1000 carmichael)
[294409,56052361,118901521,172947529,216821881,228842209,1299963601,2301745249,9624742921,11346205609,13079177569,21515221081,27278026129,65700513721,71171308081,100264053529,168003672409,172018713961,173032371289,464052305161,527519713969,663805468801,727993807201,856666552249,1042789205881,1201586232601,1396066334401,1544001719761,1797002211241,1920595706641,2028691238689,2655343122121,2718557844481,2724933935809,2920883888089,3091175755489,3267961077889,3296857440241,3414146271409,3711619793521,3719466204049,3878725359169,4287981117241,4507445537641,6323547512449,7622722964881,8544361005001 ......

findFalsePositive' 3 (take 1000 carmichael)
[294409,56052361,118901521,172947529,216821881,228842209,1299963601,2301745249,9624742921,11346205609,13079177569,21515221081,27278026129,65700513721,71171308081,100264053529,168003672409,172018713961,173032371289,464052305161,527519713969,663805468801,727993807201,856666552249,1042789205881,1201586232601,1396066334401,1544001719761,1797002211241,1920595706641,2028691238689,2655343122121,2718557844481,2724933935809,2920883888089,3091175755489,3267961077889,3296857440241,3414146271409,3711619793521,3719466204049,3878725359169,4287981117241,4507445537641,6323547512449,7622722964881,8544361005001 ......

These numbers are generated so that they will fail the test. These are nubers to prove that the test will not work. That is why it will return the same list
-}

-- Exercise 6.1
-- Time 1 hour

findFalsePositiveMr' :: Int -> [Integer] -> IO ([Integer])
findFalsePositiveMr' _ [] = return []
findFalsePositiveMr' k (check:checks) = do pTest <- primeMR k (check)
                                           if pTest then do rest <- (findFalsePositiveMr' k checks)
                                                            return (check : rest)
                                           else (findFalsePositiveMr' k checks)


{-
findFalsePositiveMr' 1 (take 100 carmichael)
[56052361,2301745249,663805468801,6323547512449,7622722964881,8681793690961,14470947115561,14685655594249,17167430884969,22027380041449,39782913594409,77833567590769,98445661027561,127393969917241,177548395641481]

findFalsePositiveMr' 2 (take 100 carmichael)
[98445661027561,390854788519609,413847154073161,5690586528027001,105293775654920089,855337156975081441,1154558595724288849,1451571149015348329,1963100046228106321,2208897084749237929]

findFalsePositiveMr' 3 (take 100 carmichael)
[]

This method is better, it give less false positives.
-}

-- Exercise 6.2
-- Time 1 hour

mersennePrimes :: Int -> [Integer] -> IO [Integer]
mersennePrimes _ [] = return []
mersennePrimes k (p:ps) = do ip <- primeMR k mPrime
                             if ip then do next' <- next
                                           return (mPrime : next')
                                   else next
                             where mPrime = (2^p) - 1
                                   next = mersennePrimes k ps

{-
mersennePrimes 10 (take 100 primes)
[3,7,31,127,8191,131071,524287,2147483647,2305843009213693951,618970019642690137449562111,162259276829213363391578010288127,170141183460469231731687303715884105727,6864797660130609714981900799081393217269435300143305409394463459185543183397656052122559640661454554977296311391480858037121987999716643812574028291115057151]

These are mersennePrimes because a mersennePrimes is in the form of (2^p)-1. Where P is a prime.
The probability is high that these are primes. There is a very small chance that you get a false Positive prime from the Miller-Rabin function.
You also check whever (2^p)-1 is a prime so you have to have two false positives.
-}

rsaPrimeList :: Integer -> IO Integer
rsaPrimeList k = do num <- randomRIO (2^(k-1), 2^k-1)
                    isPrime <- primeMR 10 num
                    if (isPrime) then return num
                                 else rsaPrimeList k

rsaPair :: Integer -> IO (Integer, Integer)
rsaPair k = do x <- rsaPrimeList k
               y <- rsaPrimeList k
               if ( x == y) then rsaPair k
                            else return (x,y)

runRSA :: (Integer,Integer) -> Integer -> IO ()
runRSA (x,y) v = do print "Encoding "
                    print v
                    print "p"
                    print x
                    print "q"
                    print y
                    print "pubKey"
                    print pubKey
                    print "privKey"
                    print privKey
                    print "encodeCip"
                    print encodeCip
                    print "cipDecode"
                    print cipDecode
                 where pubKey = rsaPublic x y
                       privKey = rsaPrivate x y
                       encodeCip = rsaEncode pubKey v
                       cipDecode = rsaDecode privKey encodeCip

doRSA k v = do (x,y) <- rsaPair k
               runRSA (x,y) v

{-
doRSA 100 10656723443534
"Encoding "
10656723443534
"p"
760312092755739826947417093877
"q"
1185039863844062541918147021493
"pubKey"
(5,901000138878256174651926595168787428987062424529721517698361)
"privKey"
(360400055551302469860770638066736830812185048864342381433197,901000138878256174651926595168787428987062424529721517698361)
"encodeCip"
422744751805116888042001903830014031821411770647595689433401
"cipDecode"
10656723443534

-}
