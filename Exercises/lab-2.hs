{-
  Abel Kaandorp (12012149)
  Laurens de Gilde (11721863)
  Laurence Saes (11422629)
  Tim Vonsee (11233001)
-}

module Lecture2

where

import System.Random
import Test.QuickCheck
import Data.List
import Data.Char



-- Lecture code

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
-- Time 4 hours

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
-- But it is not statistically significant
-- See extrs 1 proof.png



-- Exercise 2.
-- 2,5 hours

{-
Write a program (in Haskell) that takes a triple of integer values as arguments and gives as output one of the following statements:

        Not a triangle (Geen driehoek) if the three numbers cannot occur as the lengths of the sides of triangle,

        Equilateral (Gelijkzijdig) if the three numbers are the lengths of the sides of an equilateral triangle,

        Rectangular (Rechthoekig) if the three numbers are the lengths of the sides of a rectangular triangle,

        Isosceles (Gelijkbenig) if the three numbers are the lengths of the sides of an isosceles (but not equilateral) triangle,

        Other (Anders) if the three numbers are the lengths of the sides of a triangle that is not equilateral, not rectangular, and not isosceles.-}

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

{-
To test the correctness of the program, we have devised a number of cases of which the program would need to succesfully
assign to the right type of triangles. For example: A triangle with sides such as (2,2,2) needs to be assigned to
the type 'Equilateral' and a triangle with sides such as (3,4,4) needs to be assigned to the type 'Isosceles'. There are also
a number of exceptional cases which do need to be handled. The program could for example have an input which defines one or more
sides of a triangle as a negative number. In this case the program needs to correctly state that the input does not result in a
triangle. -}

triangleTest:: Bool
triangleTest = all (\(a,b,c,shape) -> triangle a b c == shape) testCases

testCases = [(2,2,2, Equilateral),
             (3,4,4, Isosceles),
             (4,3,4, Isosceles),
             (4,4,3, Isosceles),
             (3,4,5, Rectangular),
             (1,1,2, NoTriangle),
             ((-1), 1, 2, NoTriangle),
             ((-1), (-2), (-3), NoTriangle),
             (0,1,2, NoTriangle),
             (0,0,0, NoTriangle)]

-- Exercise 3: Testing property strength
-- 1,5 hours

{-a: Implement all properties from the Exercise 3 from Workshop 2 as Haskell functions of type Int -> Bool. Consider a small domain like [(−10)..10][(−10)..10].} -}

property1, property2, property3, property4 :: Int -> Bool
property1 x = even x && x > 3
property2 x = even x || x > 3
property3 x = (even x && x > 3) || even x
property4 x = (even x && x > 3) || even x

{-
-- 1
length (filter (==True) (map property1 [(-10)..10]))
4
length (filter (==True) (map even [(-10)..10]))
11

-- So property1 is stronger

-- 2
length (filter (==True) (map even [(-10)..10]))
11

length (filter (==True) (map property2 [(-10)..10]))
14

-- So even is stronger

-- 3
length (filter (==True) (map property3 [(-10)..10]))
11

length (filter (==True) (map even [(-10)..10]))
11

-- They have the same strength. So both are stronger

-- 4
length (filter (==True) (map even [(-10)..10]))
11

length (filter (==True) (map property4 [(-10)..10]))
11

-- They have the same strength. So both are stronger

So the stength list:
property1
even
property3 - property4
property2
-}

-- Exercise 4
-- time 1 hour

isPermutation :: (Ord a) => [a] -> [a] -> Bool
isPermutation xs ys = sort xs == sort ys

testPermutations :: Bool
testPermutations = all id correct && all id inCorrect
                    where correct = map ( \x -> isPermutation (fst x) (snd x) ) [([1,2,3],[3,2,1]), ([1,2,3],[2,3,1]), ([1,2,3],[1,3,2])]
                          inCorrect = map ( \x -> not (isPermutation (fst x) (snd x)) ) [([1,2,3],[3,3,1]), ([1,2,3],[1,3,1]), ([1,2,3],[1,2,1])]

noNonEqualSize :: Int -> Bool
noNonEqualSize x = isPermutation [x] [] == False && isPermutation [] [x] == False

isAllPermutationTest :: Int -> Bool
isAllPermutationTest x = all id (map ( \a -> isPermutation a [0..x]) (permutations [0..x]))

isAllNonPermutationTest :: Int -> Bool
isAllNonPermutationTest x = all id (map ( \a -> not (isPermutation a [0..x + 1])) (permutations [0..x]))

isAllNon2PermutationTest :: Int -> Bool
isAllNon2PermutationTest x = all id (map ( \a -> not (isPermutation a [1..x + 1])) (permutations [0..x]))


{-
Tests:
quickCheck noNonEqualSize
quickCheck (\(Positive x) -> isAllPermutationTest x)
quickCheck (\(Positive x) -> isAllNonPermutationTest x)
quickCheck (\(Positive x) -> isAllNon2PermutationTest x)
-}


-- When you can assume that there are no duplicates, then there are much less elements to test

-- Exercise 5
-- Time 3 hours

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

-- Exercise 6
-- Time 3 hours

{-
  Specification:
    ROT13 is a ceaser cipher that replaces letters by a leter 13 places
    further down the alphabeth. When it reaches the end of the alphabeth
    it "wraps around".

    Exmaples:
      - "Hello world" -> "Uryyb jbeyq"
-}

rot13 [] = []
rot13 (x:xs) | isLower x && (ord x + 13) >= 122  = chr ((ord x + 13) - 26) : rot13 xs
             | isUpper x && (ord x + 13) >= 90 = chr ((ord x + 13) - 26) : rot13 xs
             | isLower x && (ord x + 13) > 96 && (ord x + 13) < 123 = chr (ord x + 13)  : rot13 xs
             | isUpper x && (ord x + 13) > 64 && (ord x + 13) < 91  = chr (ord x + 13) : rot13 xs
             | otherwise = x : rot13 xs
checkRot13 xs = rot13(rot13 xs)

-- quickCheck checkRot13
rot13Test :: Bool
rot13Test = all ( \(value) -> checkRot13 value == value) rot13Cases

rot13Cases = [("Hello world"),
             ("345345"),
             ("!#$@#$")]

-- Exercise 7
-- Time 3 hours

rearrange :: [Char] -> [Char]
rearrange iban = drop 4 iban ++ take 4 iban

cleanSpaces :: [Char] -> [Char]
cleanSpaces [] = []
cleanSpaces (letter:iban) | isSpace letter = cleanSpaces iban
                          | otherwise = letter : cleanSpaces (iban)
toAscii :: Char -> [Char]
toAscii letter | isNumber letter = [letter]
               | otherwise = show ((ord letter) - 55)
convert :: [Char] -> [Char]
convert iban =  concat $ map (toAscii) . cleanSpaces . rearrange $ iban

check :: [Char] -> Bool
check iban = (read(convert iban) :: Integer) `mod` 97 == 1

-- quickCheck
ibanTest:: Bool
ibanTest = all ( \(iban,value) -> check iban == value) ibanCases

ibanCases = [("SE35 5000 0000 0549 1000 0003", True),
             ("CH93 0076 2011 6238 5295 7", True),
             ("TN59 1000 6035 1835 9847 8831", True),
             ("TR33 0006 1005 1978 6457 8413 26", True),
             ("AE07 0331 2345 6789 0123 456", True)]

-- Bonus:
-- 1 Multiples of 3 and 5 time 20 minutes
-- sum (multiple [0..999])
-- 233168
multiple = filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0 )

-- 2 Even Fibonacci numbers time 20 minutes
-- sum (filter even (takeWhile (\x -> x < 4000000) fibs))
-- 4613732

fibs = 0 : 1 : map (\x -> fst x + snd x ) (zip (fibs) (tail fibs))

-- 3 Largest prime factor time 30 minutes
-- maximum (factors 600851475143)
-- 6857

factors :: Int -> [Int]
factors 1 = []
factors n = x : factors (n `div` x) where x = factor 2 n

factor :: Int -> Int -> Int
factor x y | x > y = 1
           | y `mod` x == 0 = x
           | otherwise = factor (x+1) y



-- 4 Largest palindrome product time 1 hour
-- 906609

isPalindrome :: Int -> Bool
isPalindrome xs = length (filter (\x -> fst x /= snd x) (zip (xss) (reverse xss))) == 0 where xss = show xs

combinations :: Int -> Int -> [(Int,Int)]
combinations x y = [(a,b) | a <- [x..y], b <- [x..y] ]

multPair :: (Int,Int) -> Int
multPair x = (fst x) * (snd x)

largestPalindrome = head . filter isPalindrome . reverse . sort . map multPair
