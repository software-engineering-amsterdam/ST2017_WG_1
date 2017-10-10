module Lab6 where

import Data.List
import System.Random
import Lecture6

-- exM 2 3 4 = 2^3 mod 4
exM :: Integer -> Integer -> Integer -> Integer
exM n f m | f < 2 = (n^f) `mod` m
          | otherwise = (sqmod * sqmod) `mod` m
                          where sqmod = (n `mod` m)

{-
(x mod n * x mod n) mod n = x^2 mod n

( x^32 * x mod n ) mod n

x x^2 x^4 x^8 x^16 x^32 -> x^33
-}
