-- module Lab4 where
--
-- import Data.List
-- import System.Random
-- import Test.QuickCheck

module SetOrd (Set(..),emptySet,isEmpty,inSet,subSet,insertSet,
               deleteSet,powerSet,takeSet,(!!!),list2set,unionSet, intersectionSet, differenceSet)

where
import Test.QuickCheck
import Data.Tuple
import Data.List

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

numberSet = Set [1,2,3,4,5,6,7,8,9,10]

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

-------------- Assignment 3 ---------------
-- time spent 2 hour


unionSet :: (Ord a) => Set a -> Set a -> Set a
unionSet (Set [])     set2  =  set2
unionSet (Set (x:xs)) set2  =
    insertSet x (unionSet (Set xs) set2)


intersectionSet :: (Ord a) => Set a -> Set a -> Set a
intersectionSet (Set [])     set2  =  Set []
intersectionSet (Set (x:xs)) set2  | inSet x set2 = insertSet x (intersectionSet (Set xs) set2)
                                   | otherwise = intersectionSet (Set xs) set2

differenceSet :: (Ord a) => Set a -> Set a -> Set a
differenceSet (Set []) set2 = Set []
differenceSet (Set set1) set2  =  deleteSet' (intersectionSet (Set set1) set2) (unionSet (Set set1) set2)

deleteSet' :: Ord a => Set a -> Set a -> Set a
deleteSet' (Set (a)) (Set b) = list2set (filter (`notElem` a) b)

testSet1 = Set [1,2,3,4,5]
testSet2 = Set [3,4,5,6,7]
resultIntersectionSet12 = Set [3,4,5]
testIntersectionSet = intersectionSet testSet1 testSet2 == resultIntersectionSet12
--
testSet3 = Set [1,2,3,4,5]
testSet4 = Set [3,4,5,6,7]
resultDifferenceSet34 = Set [1,2,6,7]
testDifferencecSet = differenceSet testSet3 testSet4 == resultDifferenceSet34

-------------- Assignment 5 ---------------
-- time spent 30 minutes
type Rel a = [(a,a)]

comb :: Rel a -> Rel a -> Rel a
comb [] ys = ys
comb (x:xs) ys = x:comb ys xs

symClos :: Ord a => Rel a -> Rel a
symClos xs = comb xs swaped
            where swaped = map swap xs



-------------- Assignment 6 ---------------
-- Got the inspiration of the function from the link below. Trying to adapt the function to use the function as given in the lab excersise.
-- https://stackoverflow.com/questions/19212558/transitive-closure-from-a-list-using-haskell
-- time spent 1 hour

trClo :: Ord a => Rel a -> Rel a
trClo [] = []
trClo [x] = [x]
trClo (relation:relations) = [relation] ++ (trCloHelper [relation] [relations]) ++  (trClo relations)
trCloHelper x [y] = [(x1, y2) | (x1, x2) <- x, (y1, y2) <- trClo (y), x2 == y1]


-------------- Assignment 7 ---------------
relationOne :: Rel Int
relationOne = [(1,2),(2,3),(3,4)]

relationOneSymCloResult = [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]
relationOneTrColResult = [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

symCloTester = symClos relationOne == relationOneSymCloResult
trCloTester = trClo relationOne == relationOneTrColResult
