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



-- Exercise 2

data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

pickTriangle :: Integer -> Integer -> Integer -> Shape
pickTriangle x y z      | x == y, y == z, x == z = Equilateral
                        | (x == y || y == z || x == z) && (x+y > z) && (y+z > x) && (x+z>y) = Isosceles
                        | x^2 + y^2 == z^2 = Rectangular
                        | (x + y > z) && (x + z > y) && (y + z > x) = Other
                        | otherwise = NoTriangle

--RandomTestTriangles :: Bool
--RandomTestTriangles test = (randomR)

-- Testing
-- To test the recognition of triangles, we generate arrays consisting of 3 numbers until all types of triangles have
-- been picked at least once.


{- let testTriangles 

genRandomTestCases :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
  r  <- randomRIO (1,100)
  rs <- randomList (n-1)
  return (r:rs)
-}

triangleTest:: Bool
triangleTest = all (\(a,b,c,shape) -> pickTriangle a b c == shape) testCases


testCases = [(2,2,2, Equilateral), 
        (3,4,4, Isosceles), 
        (4,3,4, Isosceles), 
        (4,4,3, Isosceles),
        (3,4,5, Rectangular),
        (6,12,14, Rectangular),
        (1, 1000, 1, Other),
        (15, 100, 2, Other),
        (0,0,0, NoTriangle),
        (0, 1, 2, NoTriangle),
        ((-1), (-2), (-3), NoTriangle),
        ((-1), 1, 2, NoTriangle)]       

