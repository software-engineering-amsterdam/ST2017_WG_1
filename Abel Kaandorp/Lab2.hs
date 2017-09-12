module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

-- Exercise 1

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
            p <- getStdRandom random
            ps <- probs (n-1) 
            return (p:ps)

--removeMinMax :: [Int] -> [Int]
--removeMinMax xs = filter (not.flip elem [minimum xs, maximum xs]) xs

--countInRange :: [Int] -> Int
--countInRange a
--        | inRange (0,0.25) a = 1
--        | inRange (0.25 - 0.5) = 1
--        | inRange (0.5 - 0.75) = 1
--        | inRange (0.75 - 1) = 1



{- Exercise 2: Recognizing triangles

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

{- Exercise 2: Test
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

{-- Exercise 3: Testing property strength --}

{-a: Implement all properties from the Exercise 3 from Workshop 2 as Haskell functions of type Int -> Bool. Consider a small domain like [(−10)..10][(−10)..10].} -}

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

property1, property2, property3, property4 :: Int -> Bool
property1 x = even x && x > 3
property2 x = even x || x > 3
property3 x = (even x && x > 3) || even x
property4 x = even

