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

-- generatePropsI (probs 10000)
-- (2514,2584,2535,2367)
-- The distribution is very close. 25,1% - 25.84% - 25.35% - 23.67%
-- In the last slot it will 123 times

-- statistically significant
-- It should have an exqual density.
-- To disprove that the deferenince in the fractrions or significant enought that it is not
-- by accedent that they are to high.

-- standart deviation
-- 0.5
-- truly uniform

-- Exercise 2

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

-- Exercise 3

isPermutation :: (Ord a) => [a] -> [a] -> Bool
isPermutation xs ys = sort xs == sort ys

isNoSameIndexValues :: (Eq a) => [a] -> [a] -> Bool
isNoSameIndexValues xs ys = all id (zipWith (\x y -> x /= y) xs ys)

isSameLength :: [a] -> [a] -> Bool
isSameLength xs ys = length xs == length ys

isDerangement :: (Ord a) => [a] -> [a] -> Bool
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

totalDerangementsForSize :: Int -> Int
totalDerangementsForSize x = 1 -- TODO HELP

testRandomDerangement :: (Ord a) => [a] -> Bool
testRandomDerangement xs = derangements == totalDerangementsForSize (length xs)
                           where derangeList = map (\ys -> isDerangement xs ys) (permutations xs)
                                 derangements = length (filter (==True) derangeList)

-- quickCheck propertyPermutation
-- quickCheck propertyNoSameIndexValues
-- quickCheck propertyisSameLength

tpro :: Int -> Bool -> Int
tpro x f = length( filter (==f) (map (\ys -> isDerangement [1..x] ys) (permutations [1..x])))





-- Zip the lists. drop while not equal and check if empty list
-- Then foldr over list one

-- No element in the lists can be the same on the same index
-- It has to be a permutation of eachother
