module Lecture2

where

import System.Random
import Test.QuickCheck
import Data.List
import Test.QuickCheck

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

randomFlip :: Int -> IO Int
randomFlip x = do
   b <- getRandomInt 1
   if b==0 then return x else return (-x)

genIntList :: IO [Int]
genIntList = do
  k <- getRandomInt 20
  n <- getRandomInt 10
  getIntL k n

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do
   x <-  getRandomInt k
   y <- randomFlip x
   xs <- getIntL k (n-1)
   return (y:xs)

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
   quicksort [ a | a <- xs, a <= x ]
   ++ [x]
   ++ quicksort [ a | a <- xs, a > x ]

isTrue :: a -> Bool
isTrue _ = True

prop_ordered :: Ord a => [a] -> Bool
prop_ordered [] = True
prop_ordered (x:xs) = all (>= x) xs && prop_ordered xs

testR :: Int -> Int -> ([Int] -> [Int])
                    -> ([Int] -> [Int] -> Bool) -> IO ()
testR k n f r = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- genIntList
                  if r xs (f xs) then
                    do print ("pass on: " ++ show xs)
                       testR (k+1) n f r
                  else error ("failed test on: " ++ show xs)

testPost :: ([Int] -> [Int]) -> ([Int] -> Bool) -> IO ()
testPost f p = testR 1 100 f (\_ -> p)

quicksrt :: Ord a => [a] -> [a]
quicksrt [] = []
quicksrt (x:xs) =
   quicksrt [ a | a <- xs, a < x ]
   ++ [x]
   ++ quicksrt [ a | a <- xs, a > x ]

samelength :: [Int] -> [Int] -> Bool
samelength xs ys = length xs == length ys

testRl :: ([Int] -> [Int]) -> ([Int] -> [Int] -> Bool) -> IO ()
testRl f r = testR 1 100 f r

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

hoareTest :: (a -> Bool) -> (a -> a) -> (a -> Bool) -> [a] -> Bool
hoareTest precondition f postcondition =
    all (\x -> precondition x --> postcondition (f x))

hoareTestR ::  Fractional t =>
               (a -> Bool)
               -> (a -> a) -> (a -> Bool) -> [a] -> (Bool,t)
hoareTestR precond f postcond testcases = let
       a = fromIntegral (length $ filter precond testcases)
       b = fromIntegral (length testcases)
     in
       (all (\x ->
         precond x --> postcond (f x)) testcases,a/b)

invarTest :: (a -> Bool) -> (a -> a) -> [a] -> Bool
invarTest invar f = hoareTest invar f invar

invarTestR ::  Fractional t =>
               (a -> Bool) -> (a -> a) -> [a] -> (Bool,t)
invarTestR invar f = hoareTestR invar f invar

f1,f2 :: Int -> Int
f1 = \n -> sum [0..n]
f2 = \n -> (n*(n+1)) `div` 2

test = verboseCheck (\n -> n >= 0 ==> f1 n == f2 n)

parity n = mod n 2

testRel :: (a -> a -> Bool) -> (a -> a) -> [a] -> Bool
testRel spec f = all (\x -> spec x (f x))

testInvar :: Eq b => (a -> b) -> (a -> a) -> [a] -> Bool
testInvar specf = testRel (\ x y -> specf x == specf y)

stronger, weaker :: [a] ->
       (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

neg :: (a -> Bool) -> a -> Bool
neg p = \ x -> not (p x)

infixl 2 .&&.
infixl 2 .||.

(.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p .&&. q = \ x -> p x && q x

(.||.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p .||. q = \ x -> p x || q x

compar :: [a] -> (a -> Bool) -> (a -> Bool) -> String
compar xs p q = let pq = stronger xs p q
                    qp = stronger xs q p
                in
                  if pq && qp then "equivalent"
                  else if pq  then "stronger"
                  else if qp  then "weaker"
                  else             "incomparable"

infixl 2 #

(#) :: (a -> b) -> (b -> c) -> (a -> c)
(#) = flip (.)


-- -----------------------

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1)
             return (p:ps)


-- Exercise 1

generateProps :: [Float] -> (Int,Int,Int,Int)
generateProps [] = (0,0,0,0)
generateProps (x:xs) = let (q,w,e,r) = generateProps xs in
        (q + q2, w + w2, e + e2, r + r2)
          where  q2 = if (x >= 0 && x <= 0.25) then 1 else 0
                 w2 = if (x > 0.25 && x <= 0.5) then 1 else 0
                 e2 = if (x > 0.5 && x <= 0.75) then 1 else 0
                 r2 = if (x > 0.75 && x <= 1) then 1 else 0

generatePropsI :: IO [Float] -> IO (Int,Int,Int,Int)
generatePropsI fai = do fa <- fai
                        return (generateProps fa)

--  TIme 4 hours

-- generatePropsI (probs 10000)
-- (2514,2584,2535,2367)
-- The distribution is very close. 25,1% - 25.84% - 25.35% - 23.67%

-- But it is not statistically significant
-- See extrs 1 proof.png

-- Exercise 2. 1 hour

data Shape = NoTriangle | Equilateral
            | Isosceles  | Rectangular | Other deriving (Eq,Show)

isTriangle :: Integer -> Integer -> Integer -> Bool
isTriangle a b c = a + b > c && a + c > b && b + c > a && a > 0 && b > 0 && c > 0

isEquilateralTriangle :: Integer -> Integer -> Integer -> Bool
isEquilateralTriangle a b c = (isTriangle a b c) && a == b && b == c

isRectangular :: Integer -> Integer -> Integer -> Bool
isRectangular a b c = (isTriangle a b c) && a * a + b * b == c * c || a * a + c * c == b * b || b * b + c * c == a * a

isIsosceles :: Integer -> Integer -> Integer -> Bool
isIsosceles a b c = (isTriangle a b c) && not (isEquilateralTriangle a b c) && a == b || a == c || b == c

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c | not isTri = NoTriangle
               | isEqu = Equilateral
               | isRec = Rectangular
               | isIso = Isosceles
               | otherwise = Other
                   where isTri = isTriangle a b c
                         isEqu = isEquilateralTriangle a b c
                         isRec = isRectangular a b c
                         isIso = isIsosceles a b c


-- triangle 6 7 8
-- Other

-- triangle 6 6 3
-- Isosceles

-- triangle 4 4 4
-- Equilateral

-- triangle 1 1 2
-- NoTriangle

-- triangle 3 4 5
-- Rectangular

-- Exercise 3 Time 3 hours

isPermutation :: (Ord a) => [a] -> [a] -> Bool
isPermutation xs ys = sort xs == sort ys

isNoSameIndexValues :: (Eq a) => [a] -> [a] -> Bool
isNoSameIndexValues xs ys = all id (zipWith (\x y -> x /= y) xs ys)

isSameLength :: [a] -> [a] -> Bool
isSameLength xs ys = length xs == length ys

isDerangement :: (Ord a) => [a] -> [a] -> Bool
isDerangement [] [] = False
isDerangement xs ys = sameLenth && notSameIndexes && permutation
                      where sameLenth = isSameLength xs ys
                            notSameIndexes = isNoSameIndexValues xs ys
                            permutation = isPermutation xs ys

deran :: (Eq a) => [a] -> [[a]]
deran xs = filter (/=xs) (permutations xs)

propertyPermutation :: (Ord a) => [a] -> [a] -> Bool
propertyPermutation xs ys = isDerangement xs ys --> isPermutation xs ys

propertyNoSameIndexValues :: (Ord a) => [a] -> [a] -> Bool
propertyNoSameIndexValues xs ys = isDerangement xs ys --> isNoSameIndexValues xs ys

propertyisSameLength :: (Ord a) => [a] -> [a] -> Bool
propertyisSameLength xs ys = isDerangement xs ys --> isDerangement xs ys

factorial:: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Source http://oeis.org/wiki/Number_of_derangements
derSum :: Integer -> Float
derSum 1 = 0
derSum n = sum( map(\k -> ((-1)^(k) / fromIntegral((factorial k)))) [2..n])

-- Source http://oeis.org/wiki/Number_of_derangements
totalDerangementsForSize :: Integer -> Integer
totalDerangementsForSize n = round (fromIntegral (factorial n) * (derSum n))

testRandomDerangement :: [Int] -> Bool
testRandomDerangement xs = toInteger (derangements) == totalDerangementsForSize (toInteger (length xs))
                           where derangeList = map (\ys -> isDerangement xs ys) (permutations xs)
                                 derangements = length (filter (==True) derangeList)

-- quickCheck propertyPermutation
-- quickCheck propertyNoSameIndexValues
-- quickCheck propertyisSameLength
-- quickCheck testRandomDerangement
-- We calculate how many derangement there have to be on a permutation and validate this

-- 1 Multiples of 3 and 5
multiple = filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0 )
-- sum (multiple [0..999])
-- 233168

-- 2 Even Fibonacci numbers
fibs = 0 : 1 : map (\x -> fst x + snd x ) (zip (fibs) (tail fibs))
-- sum (filter even (takeWhile (\x -> x < 4000000) fibs))
-- 4613732

-- 3 Largest prime factor

factors :: Int -> [Int]
factors 1 = []
factors n = x : factors (n `div` x) where x = factor 2 n

factor :: Int -> Int -> Int
factor x y | x > y = 1
           | y `mod` x == 0 = x
           | otherwise = factor (x+1) y

-- maximum (factors 600851475143)
-- 6857

-- 4 Largest palindrome product
-- 906609

isPalindrome :: Int -> Bool
isPalindrome xs = length (filter (\x -> fst x /= snd x) (zip (xss) (reverse xss))) == 0 where xss = show xs

combinations :: Int -> Int -> [(Int,Int)]
combinations x y = [(a,b) | a <- [x..y], b <- [x..y] ]

multPair :: (Int,Int) -> Int
multPair x = (fst x) * (snd x)

largestPalindrome = head . filter isPalindrome . reverse . sort . map multPair

-- 11 Largest product in a grid

inputGritt = "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08 49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00 81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65 52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91 22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80 24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50 32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70 67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21 24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72 21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95 78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92 16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57 86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58 19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40 04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66 88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69 04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36 20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16 20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54 01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"

inputGrittToString :: String -> [Int]
inputGrittToString [] = []
inputGrittToString (x:y:zs) | length zs > 0 = read [x,y] : inputGrittToString (tail zs)
                           | otherwise = [read [x,y]]

stringToGritt :: Int -> [Int] -> [[Int]]
stringToGritt _ [] = []
stringToGritt gs xs = (take gs xs) : (stringToGritt gs (drop gs xs))

checkOnGritt = stringToGritt 20 (inputGrittToString inputGritt);

findGreatestProduct :: Int -> Int -> [[Int]] -> [Int]
findGreatestProduct 17 y xs = findGreatestProduct 0 (y+1) xs
findGreatestProduct _ 20 _ = []
findGreatestProduct x y xs = product ltor : product tltobr : product trtobl : product ttob : findGreatestProduct (x+1) y xs
                             where ltor = fromGritt (\ix iy -> (ix + 1, iy)) 4 x y xs
                                   tltobr = fromGritt (\ix iy -> (ix - 1, iy + 1)) 4 x y xs
                                   trtobl = fromGritt (\ix iy -> (ix + 1, iy + 1)) 4 x y xs
                                   ttob = fromGritt (\ix iy -> (ix, iy + 1)) 4 x y xs

fromGritt :: (Int -> Int -> (Int,Int)) -> Int -> Int -> Int -> [[Int]] -> [Int]
fromGritt _ 0 _ _ _ = []
fromGritt f num x y xs = newElement : fromGritt f (num-1) newX newY xs
                         where (newX,newY) = f x y
                               newElement = grittLookup x y xs

grittLookup :: Int -> Int -> [[Int]] -> Int
grittLookup x y xs | length xs > y && length getY > x && x >= 0 && y >= 0 = getX
                   | otherwise = 0
                     where getY = (xs !! y)
                           getX = getY !! x
