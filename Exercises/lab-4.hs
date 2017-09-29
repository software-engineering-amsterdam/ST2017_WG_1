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

-------------- Assignment 2 ---------------
-- time spent 3 hour
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
-- time spent 1 hour

relationOne :: Rel Int
relationOne = [(1,2),(2,3),(3,4)]

relationOneSymCloResult = [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]
relationOneTrColResult = [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

symCloTester = symClos relationOne == relationOneSymCloResult
trCloTester = trClo relationOne == relationOneTrColResult


-------------- Assignment 9 ---------------
-- time spent 4 hours
type Var = String
type Env = Var -> Integer

data Expr = I Integer | V Var
          | Add Expr Expr
          | Subtr Expr Expr
          | Mult Expr Expr
          deriving (Eq)

instance Show Expr where
  show (I i) = show i
  show (V v) = show v
  show (Add r l) = show l ++ " + " ++ show r
  show (Subtr l r) = show l ++ " - " ++ show r
  show (Mult l r) = show l ++ " * " ++ show r

data Condition = Prp Var
               | Eq Expr Expr
               | Lt Expr Expr
               | Gt Expr Expr
               | Ng Condition
               | Cj [Condition]
               | Dj [Condition]
               deriving (Eq)

instance Show Condition where
  show (Prp i) = show i
  show (Eq r l) = show l ++ " == " ++ show r
  show (Lt l r) = show l ++ " > " ++ show r
  show (Gt l r) = show l ++ " < " ++ show r
  show (Ng i) = "!" ++ show i
  show (Cj i) = intercalate " && " (map show i)
  show (Dj i) = intercalate " || " (map show i)

data Statement = Ass Var Expr
               | Cond Condition Statement Statement
               | Seq [Statement]
               | While Condition Statement
               deriving (Eq)

instance Show Statement where
  show (Ass v e) = show v ++ " = " ++ show e
  show (Cond c sl sr) = "if " ++ (show c) ++ " then " ++ (show sl) ++ " else " ++ (show sr)
  show (Seq st) = intercalate ";\n" (map show st)
  show (While c s) = "while (" ++ show c ++ ") then {\n" ++ show s ++ "\n}"

fib :: Statement
fib = Seq [Ass "x" (I 0), Ass "y" (I 1),
           While (Gt (V "n") (I 0))
             (Seq [Ass "z" (V "x"),
                   Ass "x" (V "y"),
                   Ass "y" (Add (V "z") (V "y")),
                   Ass "n" (Subtr (V "n") (I 1))])]

