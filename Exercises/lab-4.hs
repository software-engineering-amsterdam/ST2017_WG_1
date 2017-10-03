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
import System.Random

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



-------------- Assignment 1 ---------------
-- Read or reread Chapter 4 of The Haskell Road, and make
-- a list of questions on specific points that cause difficulty of understanding.
-- Time spent:     2 hours

{-  ### Chapter 4: Sets, Types and Lists ### -}
{-  1.  The book states that a /= a in most cases. It then gives an example of a case
        where it is useful to have sets have themselves as members. This example concerns
        infinite streams. Are there any other cases where it is useful to have sets have
        themselves as member? How are these cases useful?
        (Page 125, 4.3 Haskell road to logic)
    2.  We don't get the way the translation in example 4.25 is formed. How does the book get to this
        logical form from the prevoiusly defined sets and formula?
        (Page 134, 4.4 Haskell road to logic)
    3.  Definition 4.42 states that pairs have predefined functions fst and snd to get the first and
        second member of a pair respectively. In the case of a triple or quadruple collection of
        elements, are there also functions to get the third of fourth elements, besides functions for
        getting a specific element x.?
        (Page 139, 4.5 Haskell road to logic)  -}

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
-- time spent 4 hour


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

-- Get set inverse
getSetInverse :: (Enum a, Ord a) => Set a -> Set a
getSetInverse (Set []) = emptySet
getSetInverse (Set xs) = list2set ([i | i <- [min..max], elem i xs == False ])
                            where min = minimum xs
                                  max = maximum xs


combindSet :: Ord t => Set t -> Set t -> Set t
combindSet setA setB = totalSet
                        where (Set xs) = setA
                              (Set ys) = setB
                              totalSet = list2set (concat [xs,ys])

-- Test properties

-- Manual test
randomSetIO = createTestSet 0 1000 50

-- Insersection of 2 sets is the same set
propISameSet :: Ord a => Set a -> Bool
propISameSet randomSet = (intersectionSet randomSet randomSet == randomSet)

-- Intersection of a set inverse with the set is the empty set
propIOIIsEmpty :: (Enum a, Ord a) => Set a -> Bool
propIOIIsEmpty randomSet = intersectionSet (getSetInverse randomSet) randomSet == emptySet

-- Union of the same set A and B is the same as the set A and B
propSUIsSame :: Ord a => Set a -> Bool
propSUIsSame randomSet = unionSet randomSet randomSet == randomSet

-- Union of A set with the inverse of the set is all element of both sets
propUWOIT :: (Enum t, Ord t) => Set t -> Bool
propUWOIT randomSet = unionSet oSet randomSet == totalSet
                      where oSet = getSetInverse randomSet
                            (Set xs) = oSet
                            (Set ys) = randomSet
                            totalSet = combindSet oSet randomSet

-- The diference of a set inverse is the empty set
propDifOpIsTotal randomSet = differenceSet randomSet oSet == combindSet randomSet oSet
                              where oSet = (getSetInverse randomSet)


--propertyTests :: (Enum a, Ord a) => IO (Set a) -> IO ()
propertyTests setToTest = do randomSet <- setToTest
                             print "Insersection of 2 sets is the same set"
                             print (propISameSet randomSet)
                             print "Intersection of a set inverse with the set is the empty set"
                             print (propIOIIsEmpty randomSet)
                             print "Union of the same set A and B is the same as the set A and B"
                             print (propSUIsSame randomSet)
                             print "Union of A set with the inverse of the set is all element of both sets"
                             print (propUWOIT randomSet)
                             print "The diference of a set inverse is the empty set"
                             print (propDifOpIsTotal randomSet)

{-
Manual test:
  propertyTests randomSetIO
  "Insersection of 2 sets is the same set"
  True
  "Intersection of a set inverse with the set is the empty set"
  True
  "Union of the same set A and B is the same as the set A and B"
  True
  "Union of A set with the inverse of the set is all element of both sets"
  True
  "The diference of a set inverse is the empty set"
  True
-}

qPropertyTests :: (Enum a, Ord a) => (Set a) -> Bool
qPropertyTests setToTest =  (propISameSet setToTest) &&
                            (propIOIIsEmpty setToTest) &&
                            (propSUIsSame setToTest) &&
                            (propUWOIT setToTest) &&
                            (propDifOpIsTotal setToTest)


-- QuickCheck

{-
QuickCheck:
quickCheckWith stdArgs {maxSize=1000} $ forAll (sized setGenerator) qPropertyTests
-}

-------------- Assignment 4 ---------------
-- Read or reread Chapter 4 of The Haskell Road, and make
-- a list of questions on specific points that cause difficulty of understanding.
-- Time spent:     2 hours and 15 minutes


  {-  ### Chapter 4: Relations ### -}
  {-  1.   Why is the relation described in example 5.63 equivalent? The argument
         given generates some confusion.
      2.   We find the concept of equivalence classes and partitions in section 5.6
         a bit confusing.
  -}

-------------- Assignment 5 ---------------
-- time spent 30 minutes
type Rel a = [(a,a)]

comb :: (Eq a) => Rel a -> Rel a -> Rel a
comb [] ys = ys
comb (x:xs) ys = if elem x rest then rest else x:rest
                where rest = comb ys xs

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

relationTwo :: Rel Int
relationTwo = [(1,2),(3,1),(3,4)]

relationOneSymCloResult = [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]
relationOneTrColResult = [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

symCloTester = symClos relationOne == relationOneSymCloResult
trCloTester = trClo relationOne == relationOneTrColResult

-- The symmetric closure: When (a,b) is in the Rel then (b,a) is also in the Rel
propCheckReverse :: (Eq a, Foldable t) => t (a, a) -> Bool
propCheckReverse rel = all (\x -> elem (swap x) rel) rel

-- The intersection of two transitive relations is also transitive.
propTransInt :: Ord a => Rel a -> Rel a -> Bool
propTransInt tra trb = trClo tri == tri
                          where tri = filter (\x -> elem x trb) tra

-- Test generators
createRelationFromList :: [t] -> [(t, t)]
createRelationFromList [] = []
createRelationFromList [a] = []
createRelationFromList (x:y:zs) = (x,y) : createRelationFromList zs

{-
createTestRel 0 100 10
[(6,83),(40,22),(56,96),(67,79),(92,84)]
-}
createTestRel :: Int -> Int -> Int -> IO [(Int, Int)]
createTestRel mn mx c | mx - mn < c = do return []
                      | otherwise = do randomItems <- getRandomItems mn mx c
                                       return (createRelationFromList randomItems)

-- Quick check generator
relGenerator :: Int -> Gen [(Int, Int)]
relGenerator 0 = do return []
relGenerator n | n>0 = do a <- arbitrary
                          return a

-- quickCheckWith stdArgs {maxSize=1000} $ forAll (sized relGenerator) someTest

testProp1 = createTestRel 0 200 25
testProp2 = createTestRel 0 200 25

-- Manual test

{-
  propertyRelTests testProp1 testProp2
  "The symmetric closure: When (a,b) is in the Rel then (b,a) is also in the Rel"
  True
  "The intersection of two transitive relations is also transitive."
  True
-}
propertyRelTests relAIO relBIO = do relA <- relAIO
                                    relB <- relBIO
                                    print "The symmetric closure: When (a,b) is in the Rel then (b,a) is also in the Rel"
                                    print (propCheckReverse (symClos relA))
                                    print "The intersection of two transitive relations is also transitive."
                                    print (propTransInt (trClo relA) (trClo relB))

-- propertyRelTests testProp1 testProp2

-- quickCheck test

-- quickCheckWith stdArgs {maxSize=20} $ forAll (sized relGenerator) qRelPropertyTests

{-
  quickCheckWith stdArgs {maxSize=20} $ forAll (sized relGenerator) qRelPropertyTests
  +++ OK, passed 100 tests.
-}
qRelPropertyTests :: Ord a => Rel a -> Rel a -> Bool
qRelPropertyTests relToTesta relToTestb = (propCheckReverse (symClos relToTesta))  &&
                                          (propTransInt (trClo relToTesta) (trClo relToTestb))


-------------- Assignment 8 ---------------

-- Is there a difference between
-- the symmetric closure of the transitive closure of a relation RR
-- and the transitive closure of the symmetric closure of R?
-- Time spent: 1 hour

{- Function to check if they are the same -}
eqClosure :: Rel Int -> Bool
eqClosure r = (symClos (trClo r)) == (trClo (symClos r))


symTransClos, transSymClos :: Rel Int -> IO()
symTransClos r = print (symClos (trClo r))
transSymClos r = print (trClo (symClos r))

-- Run symTraSameAsTraSymCon to compare
symTraSameAsTraSym r = symClos (trClo r) == trClo (symClos r)
symTraSameAsTraSymCon = symTraSameAsTraSym [(1,2)]

  {-  If we use the following input:
      [(1,2)]
      The function 'eqClosure' returns 'false'

      Using the function symTransClos we get: [(1,2),(2,1)]
      Using the function transSymClos we get: [(1,2),(1,1),(2,1)]

      So we can conclude that there is in fact a difference between
      the symmetric closure of a relation R and the transitive closure
      of the symmetric closure of R.
  -}


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

-- TO show the structure just run fib

-- Euler 13
-- Time spent 4 hours
inputList11 = "37107287533902102798797998220837590246510135740250463769376774900097126481248969700780504170182605387432498619952474105947423330951305812372661730962991942213363574161572522430563301811072406154908250230675882075393461711719803104210475137780632466768926167069662363382013637841838368417873436172675728112879812849979408065481931592621691275889832738442742289174325203219235894228767964876702721893184745144573600130643909116721685684458871160315327670386486105843025439939619828917593665686757934951621764571418565606295021572231965867550793241933316490635246274190492910143244581382266334794475817892575867718337217661963751590579239728245598838407582035653253593990084026335689488301894586282278288018119938482628201427819413994056758715117009439035398664372827112653829987240784473053190104293586865155060062958648615320752733719591914205172558297169388870771546649911559348760353292171497005693854370070576826684624621495650076471787294438377604532826541087568284431911906346940378552177792951453612327252500029607107508256381565671088525835072145876576172410976447339110607218265236877223636045174237069058518606604482076212098132878607339694128114266041808683061932846081119106155694051268969251934325451728388641918047049293215058642563049483624672216484350762017279180399446930047329563406911573244438690812579451408905770622942919710792820955037687525678773091862540744969844508330393682126183363848253301546861961243487676812975343759465158038628759287849020152168555482871720121925776695478182833757993103614740356856449095527097864797581167263201004368978425535399209318374414978068609844840309812907779179908821879532736447567559084803087086987551392711854517078544161852424320693150332599594068957565367821070749269665376763262354472106979395067965269474259770973916669376304263398708541052684708299085211399427365734116182760315001271653786073615010808570091499395125570281987460043753582903531743471732693212357815498262974255273730794953759765105305946966067683156574377167401875275889028025717332296191766687138199318110487701902712526768027607800301367868099252546340106163286652636270218540497705585629946580636237993140746255962240744869082311749777923654662572469233228109171419143028819710328859780666976089293863828502533340334413065578016127815921815005561868836468420090470230530811728164304876237919698424872550366387845831148769693215490281042402013833512446218144177347063783299490636259666498587618221225225512486764533677201869716985443124195724099139590089523100588229554825530026352078153229679624948164195386821877476085327132285723110424803456124867697064507995236377742425354112916842768655389262050249103265729672370191327572567528565324825826546309220705859652229798860272258331913126375147341994889534765745501184957014548792889848568277260777137214037988797153829820378303147352772158034814451349137322665138134829543829199918180278916522431027392251122869539409579530664052326325380441000596549391598795936352974615218550237130764225512118369380358038858490341698116222072977186158236678424689157993532961922624679571944012690438771072750481023908955235974572318970677254791506150550495392297953090112996751986188088225875314529584099251203829009407770775672113067397083047244838165338735023408456470580773088295917476714036319800818712901187549131054712658197623331044818386269515456334926366572897563400500428462801835170705278318394258821455212272512503275512160354698120058176216521282765275169129689778932238195734329339946437501907836945765883352399886755061649651847751807381688378610915273579297013376217784275219262340194239963916804498399317331273132924185707147349566916674687634660915035914677504995186714302352196288948901024233251169136196266227326746080059154747183079839286853520694694454072476841822524674417161514036427982273348055556214818971426179103425986472045168939894221798260880768528778364618279934631376775430780936333301898264209010848802521674670883215120185883543223812876952786713296124747824645386369930090493103636197638780396218407357239979422340623539380833965132740801111666627891981488087797941876876144230030984490851411606618262936828367647447792391803351109890697907148578694408955299065364044742557608365997664579509666024396409905389607120198219976047599490197230297649139826800329731560371200413779037855660850892521673093931987275027546890690370753941304265231501194809377245048795150954100921645863754710598436791786391670211874924319957006419179697775990283006991536871371193661495281130587638027841075444973307840789923115535562561142322423255033685442488917353448899115014406480203690680639606723221932041495354150312888033953605329934036800697771065056663195481234880673210146739058568557934581403627822703280826165707739483275922328459417065250945123252306082291880205877731971983945018088807242966198081119777158542502016545090413245809786882778948721859617721078384350691861554356628840622574736922845095162084960398013400172393067166682355524525280460972253503534226472524250874054075591789781264330331690"

inputToList :: [Char] -> [Integer]
inputToList [] = []
inputToList xs = read (take 50 xs) : inputToList (drop 50 xs)

collatzProblem :: Int -> Int
collatzProblem n | even n = n `div` 2
                 | otherwise = 3*n + 1

calculateChain :: Int -> Int
calculateChain 1 = 1
calculateChain n = 1 + calculateChain (collatzProblem n)

-- maximum (map (\x -> ((calculateChain x),x) ) [1..999999])

main :: IO ()
main = print(maximum (map (\x -> ((calculateChain x),x) ) [1..999999]))
-- Use the ghc compiler!
