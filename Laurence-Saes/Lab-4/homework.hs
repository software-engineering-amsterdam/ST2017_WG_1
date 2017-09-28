module SetOrd (Set(..),emptySet,isEmpty,inSet,subSet,insertSet,
               deleteSet,powerSet,takeSet,(!!!),list2set,unionSet)

where

import Test.QuickCheck
import Data.List
import System.Random
import Data.Char
import Control.Monad

{-- Sets implemented as ordered lists without duplicates --}

newtype Set a = Set [a] deriving (Eq,Ord)

instance (Show a) => Show (Set a) where
    showsPrec _ (Set s) str = showSet s str

showSet []     str = showString "{}" str
showSet (x:xs) str = showChar '{' ( shows x ( showl xs str))
     where showl []     str = showChar '}' str
           showl (x:xs) str = showChar ',' (shows x (showl xs str))

emptySet  :: Set a
emptySet = Set []

isEmpty  :: Set a -> Bool
isEmpty (Set []) = True
isEmpty _        = False

inSet  :: (Ord a) => a -> Set a -> Bool
inSet x (Set s) = elem x (takeWhile (<= x) s)

subSet :: (Ord a) => Set a -> Set a -> Bool
subSet (Set []) _       = True
subSet (Set (x:xs)) set = (inSet x set) && subSet (Set xs) set

insertSet :: (Ord a) => a -> Set a -> Set a
insertSet x (Set s) = Set (insertList x s)

insertList x [] = [x]
insertList x ys@(y:ys') = case compare x y of
                                 GT -> y : insertList x ys'
                                 EQ -> ys
                                 _  -> x : ys

deleteSet :: Ord a => a -> Set a -> Set a
deleteSet x (Set s) = Set (deleteList x s)

deleteList x [] = []
deleteList x ys@(y:ys') = case compare x y of
                                 GT -> y : deleteList x ys'
                                 EQ -> ys'
                                 _  -> ys

list2set :: Ord a => [a] -> Set a
list2set [] = Set []
list2set (x:xs) = insertSet x (list2set xs)
-- list2set xs = Set (foldr insertList [] xs)

powerSet :: Ord a => Set a -> Set (Set a)
powerSet (Set xs) =
   Set (sort (map (\xs -> (list2set xs)) (powerList xs)))

powerList  :: [a] -> [[a]]
powerList  [] = [[]]
powerList  (x:xs) = (powerList xs)
                     ++ (map (x:) (powerList xs))

takeSet :: Eq a => Int -> Set a -> Set a
takeSet n (Set xs) = Set (take n xs)

infixl 9 !!!

(!!!) :: Eq a => Set a -> Int -> a
(Set xs) !!! n = xs !! n

unionSet :: (Ord a) => Set a -> Set a -> Set a
unionSet (Set [])     set2  =  set2
unionSet (Set (x:xs)) set2  =
   insertSet x (unionSet (Set xs) set2)


-- Exercise 2 -- Time 3 hours --
-- Creates a random Set with a min and max value for each value inside the set
-- A maximum number of elements inside the set
-- When it is inposible to create a set (0 till 10 and 11 items) then an empty set is returned
createTestSet :: Int -> Int -> Int -> IO (Set Int)
createTestSet mn mx c | mx - mn < c = do return emptySet
                      | otherwise = do randomSet <- getRandomItems mn mx c
                                       return (list2set randomSet)

getRandomItems :: Int -> Int -> Int -> IO ([Int])
getRandomItems _ _ 0 = do return []
getRandomItems mn mx c = do rn <- randomRIO (mn,mx)
                            nxt <- getRandomItems mn mx (c-1)
                            if elem rn nxt
                              then (getRandomItems mn mx (c))
                              else return (rn : nxt)

-- createTestSet 0 100 50

-- http://www.cse.chalmers.se/~rjmh/QuickCheck/manual_body.html
setGenerator :: Int -> Gen (Set Int)
setGenerator 0 = do return emptySet
setGenerator n | n>0 = do as <- arbitrary
                          return (list2set as)

-- To test the sets
isUnique :: [Int] -> Bool
isUnique [] = True
isUnique [_] = True
isUnique (x:y:zs) = x /= y && isUnique zs

isSet :: (Set Int) -> Bool
isSet (Set a) = isUnique a

testSet :: IO (Set Int) -> IO Bool
testSet ist = do st <- ist
                 return (isSet st)

-- Normal test:
-- testSet (createTestSet 0 1000 50)

-- QuickCheck
-- quickCheckWith stdArgs {maxSize=1000} $ forAll (sized setGenerator) isSet




-- Exercise 9
