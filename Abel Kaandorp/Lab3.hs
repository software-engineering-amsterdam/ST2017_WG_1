module Lab3 where
    
import Lecture3
import Data.List
import Data.Char
import System.Random
import Control.Monad
import Test.QuickCheck

{-  Exercise 1
    Time spent: 2 hours
    Summary:    Give definitions for various statements and provide a method of testing -}

contradiction :: Form -> Bool
contradiction n = all (\ m -> not(evl m n)) (allVals n)

tautology :: Form -> Bool
tautology n = all (`evl` n) (allVals n)

entails :: Form -> Form -> Bool
entails n m = tautology (Impl n m)

equiv :: Form -> Form -> Bool
equiv n m = entails n m &&  entails m n

{-  Test:
    Reference: Lecture3.hs
    form1 = Equiv (Impl p q) (Impl (Neg q) (Neg p))
    form2 = Equiv (Impl p q) (Impl (Neg p) (Neg q))
    form3 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r) -}

{-  We test the definitions by using the forms defined in 'Lecture3.hs'. We use these forms and their
    outcome with regards to the definitions, to show that the definitions show the right output -}

testContradiction, testTautology, testEntails, testEquivalence  ::  Bool
testContradiction = not (contradiction form1) && not(contradiction form2) && not(contradiction form3)
testTautology = tautology form1 && not(tautology form2) && tautology form3
testEntails = not(entails form1 form2)  && entails form2 form3 && entails form1 form3
testEquivalence = not(equiv form1 form2) && not(equiv form2 form3) && equiv form1 form3

testAllDefinitions = testContradiction && testTautology && testEntails && testEquivalence

{-  Exercise 2
    Time spent: /
    Summary:    The lecture notes of this week define a function 
                parse for parsing propositional formulas. Test this function. 
                You can use any test method you want.-}

--testParse :: Int -> Bool


{-  Exercise 3 
    Time spent: 4 hours
    Summary:    Write a Haskell program for converting formulas into CNF. -}

-- Will merge all cnj 's inside each other
mergeCnj :: Form -> Form
mergeCnj (Cnj []) = (Cnj [])
mergeCnj (Cnj (x:xs)) = case x of (Cnj ys) -> mergeCnj (Cnj (xs ++ ys))
                                  otherwise -> Cnj (x : next)
                                                where (Cnj next) = mergeCnj (Cnj xs)
mergeCnj a = a


-- Converts disjunction to conjunction:
-- A disjunction of 2 conjunctions are distributed
-- A non disjunction is distrubuted over the conjunction
-- Two non disjunctions will keep the same

dsjToCnj :: Form -> Form -> Form
dsjToCnj (Cnj a) (Cnj b) = (Cnj [ Dsj ([subA,subB]) | subA <- da, subB <- db ])
                            where da = map convertToCnf a
                                  db = map convertToCnf b
dsjToCnj subA (Cnj b) = (Cnj [ Dsj ([da,subB]) | subB <- db ])
                            where db = map convertToCnf b
                                  da = convertToCnf subA
dsjToCnj (Cnj a) subB = dsjToCnj subB (Cnj a)
dsjToCnj subA subB = Dsj [db, da]
                            where db = convertToCnf subB
                                  da = convertToCnf subA

-- Two conjunctions will be concatenated
-- One non conjections with a conjections will be appended
-- Two non conjunctions will heep the same
cnjToCnj :: Form -> Form -> Form
cnjToCnj (Cnj a) (Cnj b) = mergeCnj (Cnj (concat [normalCnjA,normalCnjB]))
                            where normalCnjA = map (convertToCnf) a
                                  normalCnjB = map (convertToCnf) b
cnjToCnj a (Cnj b) = mergeCnj (Cnj (normalCnjA : normalCnjB))
                            where normalCnjA = convertToCnf a
                                  normalCnjB = map (mergeCnj . convertToCnf) b
cnjToCnj (Cnj a) b = cnjToCnj b (Cnj a)
cnjToCnj a b = mergeCnj (Cnj [normalCnjA,normalCnjB])
                            where normalCnjA = convertToCnf a
                                  normalCnjB = convertToCnf b

-- A property and a negotion with a property is allowed
-- Negations with a negation will cancle eachother out
-- Negation on a conjunctions will distribute over all the sub expressions
-- Negation on a disjunction will distribute over all the sub expressions
-- An implication and equalivinze will be converted to a disjunction

convertToCnf :: Form -> Form
convertToCnf (Prop a) = Prop a
convertToCnf (Neg (Prop a)) = (Neg (Prop a))
convertToCnf (Neg (Neg a)) = convertToCnf a
convertToCnf (Neg (Cnj a)) = convertToCnf (Dsj (map (\x -> Neg x) a))
convertToCnf (Neg (Dsj a)) = convertToCnf (Cnj (map (\x -> Neg x) a))
convertToCnf (Neg a) = convertToCnf (Neg (arrowfree a))
convertToCnf (Cnj []) = (Cnj [])
convertToCnf (Cnj [a]) = convertToCnf a
convertToCnf (Cnj (a:b:cs)) | length cs == 0 = cnjAB
                            | otherwise = convertToCnf (Cnj (cnjAB:cs))
                                      where cnjAB = cnjToCnj a b
convertToCnf (Dsj []) = (Dsj [])
convertToCnf (Dsj [a]) = convertToCnf a
convertToCnf (Dsj (a:b:cs)) | length cs == 0 = dsjAB
                            | otherwise = convertToCnf (Dsj (dsjAB:cs))
                                  where suba = convertToCnf a
                                        subb = convertToCnf b
                                        dsjAB = dsjToCnj suba subb
convertToCnf (Impl a b) = convertToCnf (arrowfree (Impl a b))
convertToCnf (Equiv a b) = convertToCnf (arrowfree (Equiv a b))

{-  Exercise 4
    Time spent: /
    Summary:    Write a formula generator for random testing of properties of propositional logic, 
                or teach yourself enough QuickCheck to use random QuickCheck testing of formulas.
-}

removeArrows :: Form -> Bool
removeArrows n = n == arrowfree n

-- convertToNnf :: Form -> Form
-- convertToNnf 

eqProp :: Form -> Bool
eqProp f = equiv f (convertToCnf f)

generateForm :: Int -> Gen Form
generateForm 0 = fmap Prop (choose (0, 10))
generateForm n = oneof [ fmap Prop (choose (0, 10))
                            , fmap  Neg (generateForm m)
                            , fmap  Cnj (vectorOf n (generateForm m))
                            , fmap  Dsj (vectorOf n (generateForm m))
                            , liftM2 Impl (generateForm m) (generateForm m)
                            , liftM2 Equiv (generateForm m) (generateForm m) ]
                    where m = n - 1

testQuickCheck = quickCheck(forAll (generateForm 1) eqProp)