module Lab2 where

import Data.List
import Data.Char
import Data.Char
import System.Random
import Test.QuickCheck



---------- Lab excersise 1 ----------

---------- Lab excersise 2 ----------


data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Int -> Int -> Int -> Shape

triangle x y z | x == y && y == z && x == z = Equilateral
               | (x == y && x /= z) || (y == z && y /= x) = Rectangular
               | x == y || x == z || z == y = Isosceles
               | otherwise = NoTriangle

---------- Strengthen excersise ----------

---------- ROT13 ----------
rot13 [] = []
rot13 (x:xs) | isLower x && (ord x + 13) >= 122  = chr ((ord x + 13) - 26) : rot13 xs
             | isUpper x && (ord x + 13) >= 90 = chr ((ord x + 13) - 26) : rot13 xs
             | isLower x && (ord x + 13) > 96 && (ord x + 13) < 123 = chr (ord x + 13)  : rot13 xs
             | isUpper x && (ord x + 13) > 64 && (ord x + 13) < 91  = chr (ord x + 13) : rot13 xs
             | otherwise = x : rot13 xs
checkRot13 xs = xs == rot13(rot13 xs)

 ---------- IBAN excersise ----------

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
