module Lab2 
where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

-- lists of floating point numbers
smallerThen25 x = x < 0.25
from25to50 x = x >= 0.25 && x < 0.50
from50to75 x = x >= 0.5 && x < 0.75
from75to1 x = x >= 0.75 && x <= 1

lengthOfSub25 ns = length (filter smallerThen25 ns)
lengthOf25to50 ns = length (filter from25to50 ns)
lengthOf50to75 ns = length (filter from50to75 ns)
lengthOf75to1 ns = length (filter from75to1 ns)

countQuartiles :: IO [Float] -> IO (Int, Int, Int, Int)
countQuartiles ns = do 
                xs <- ns
                return (
                  (lengthOf25to50 xs), 
                  (lengthOf25to50 xs), 
                  (lengthOf50to75 xs),
                  (lengthOf75to1 xs)
                  )


-------------- Implementing and testing ROT13 encoding --------------
{-
  Specification:
    ROT13 is a ceaser cipher that replaces letters by a leter 13 places
    further down the alphabeth. When it reaches the end of the alphabeth
    it "wraps around".

    Exmaples: 
      - "Hello world" -> "Uryyb jbeyq"
-}

rotate :: Int -> Char -> Char
rotate n c  | isLower c && rotatedOrd >= oz = chr((rotatedOrd `mod` oz) + 96)
            | isUpper c && rotatedOrd >= oZ = chr((rotatedOrd `mod` oZ) + 64)
            | isAlpha c = chr(rotatedOrd)
            | otherwise = c
            where
              rotatedOrd = ord c + n
              oz = ord 'z'
              oZ = ord 'Z'
              

rot13 :: [Char] -> [Char]
rot13 xs = map (rotate 13) xs


