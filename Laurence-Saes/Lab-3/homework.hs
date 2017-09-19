
module Lecture3

where

import Data.List
import Data.Char
import Test.QuickCheck
import Control.Monad
import System.Random

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

infixl 2 #

(#) :: (a -> b) -> (b -> c) -> (a -> c)
(#) = flip (.)

infixl 1 $$

($$) :: a -> (a -> b) -> b
($$) = flip ($)

update :: Eq a => (a -> b) -> (a,b) -> a -> b
update f (x,y) = \ z -> if x == z then y else f z

updates :: Eq a => (a -> b) -> [(a,b)] -> a -> b
updates = foldl update

type Var = String
type Env = Var -> Integer

data Expr = I Integer
          | V Var
          | Add Expr Expr
          | Subtr Expr Expr
          | Mult Expr Expr
          deriving (Eq,Show)

eval :: Expr -> Env -> Integer
eval (I i) _ = i
eval (V name) env = env name
eval (Add e1 e2) env = (eval e1 env) + (eval e2 env)
eval (Subtr e1 e2) env = (eval e1 env) - (eval e2 env)
eval (Mult e1 e2) env = (eval e1 env) * (eval e2 env)

assign :: Var -> Expr -> Env -> Env
assign var expr env =  update env (var, eval expr env)

initEnv :: Env
initEnv = \ _ -> undefined

initE :: Env
initE = const undefined

example = initEnv $$
          assign "x" (I 3) #
          assign "y" (I 5) #
          assign "x" (Mult (V "x") (V "y")) #
          eval (V "x")

while :: (a -> Bool) -> (a -> a) -> a -> a
while = until . (not.)

euclid m n = (m,n) $$
   while (\ (x,y) -> x /= y)
         (\ (x,y) -> if x > y then (x-y,y)
                              else (x,y-x)) #
         fst

euclid' m n = fst $ eucl (m,n) where
     eucl = until (uncurry  (==))
         (\ (x,y) -> if x > y then (x-y,y) else (x,y-x))

whiler :: (a -> Bool) -> (a -> a) -> (a -> b) -> a -> b
whiler p f r = while p f # r

euclid2 m n = (m,n) $$
          whiler (\ (x,y) -> x /= y)
                 (\ (x,y) -> if x > y then (x-y,y)
                                      else (x,y-x))
                 fst

fibonacci :: Integer -> Integer
fibonacci n = fibon (0,1,n) where
  fibon = whiler
           (\ (_,_,n) -> n > 0)
           (\ (x,y,n) -> (y,x+y,n-1))
           (\ (x,_,_) -> x)

fb :: Integer -> Integer
fb n = fb' 0 1 n where
   fb' x y 0 = x
   fb' x y n = fb' y (x+y) (n-1)

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

type Name = Int

data Form = Prop Name
          | Neg  Form
          | Cnj [Form]
          | Dsj [Form]
          | Impl Form Form
          | Equiv Form Form
          deriving (Eq,Ord)

instance Show Form where
  show (Prop x)   = show x
  show (Neg f)    = '-' : show f
  show (Cnj fs)     = "*(" ++ showLst fs ++ ")"
  show (Dsj fs)     = "+(" ++ showLst fs ++ ")"
  show (Impl f1 f2)  = "(" ++ show f1 ++ "==>"
                           ++ show f2 ++ ")"
  show (Equiv f1 f2)  = "(" ++ show f1 ++ "<=>"
                           ++ show f2 ++ ")"

showLst,showRest :: [Form] -> String
showLst [] = ""
showLst (f:fs) = show f ++ showRest fs
showRest [] = ""
showRest (f:fs) = ' ': show f ++ showRest fs

p = Prop 1
q = Prop 2
r = Prop 3

form1 = Equiv (Impl p q) (Impl (Neg q) (Neg p))
form2 = Equiv (Impl p q) (Impl (Neg p) (Neg q))
form3 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r)

propNames :: Form -> [Name]
propNames = sort.nub.pnames where
  pnames (Prop name) = [name]
  pnames (Neg f)  = pnames f
  pnames (Cnj fs) = concatMap pnames fs
  pnames (Dsj fs) = concatMap pnames fs
  pnames (Impl f1 f2)  = concatMap pnames [f1,f2]
  pnames (Equiv f1 f2) = concatMap pnames [f1,f2]

type Valuation = [(Name,Bool)]

-- | all possible valuations for lists of prop letters
genVals :: [Name] -> [Valuation]
genVals [] = [[]]
genVals (name:names) =
  map ((name,True) :) (genVals names)
  ++ map ((name,False):) (genVals names)

-- | generate all possible valuations for a formula
allVals :: Form -> [Valuation]
allVals = genVals . propNames

type ValFct = Name -> Bool

val2fct :: Valuation -> ValFct
val2fct = updates (\ _ -> undefined)

fct2val :: [Name] -> ValFct -> Valuation
fct2val domain f = map (\x -> (x,f x)) domain

evl :: Valuation -> Form -> Bool
evl [] (Prop c)    = error ("no info: " ++ show c)
evl ((i,b):xs) (Prop c)
     | c == i    = b
     | otherwise = evl xs (Prop c)
evl xs (Neg f)  = not (evl xs f)
evl xs (Cnj fs) = all (evl xs) fs
evl xs (Dsj fs) = any (evl xs) fs
evl xs (Impl f1 f2) = evl xs f1 --> evl xs f2
evl xs (Equiv f1 f2) = evl xs f1 == evl xs f2

data Token
      = TokenNeg
      | TokenCnj
      | TokenDsj
      | TokenImpl
      | TokenEquiv
      | TokenInt Int
      | TokenOP
      | TokenCP
 deriving (Show,Eq)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) | isSpace c = lexer cs
             | isDigit c = lexNum (c:cs)
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs
lexer ('*':cs) = TokenCnj : lexer cs
lexer ('+':cs) = TokenDsj : lexer cs
lexer ('-':cs) = TokenNeg : lexer cs
lexer ('=':'=':'>':cs) = TokenImpl : lexer cs
lexer ('<':'=':'>':cs) = TokenEquiv : lexer cs
lexer (x:_) = error ("unknown token: " ++ [x])

lexNum cs = TokenInt (read num) : lexer rest
     where (num,rest) = span isDigit cs

type Parser a b = [a] -> [(b,[a])]

succeed :: b -> Parser a b
succeed x xs = [(x,xs)]

parseForm :: Parser Token Form
parseForm (TokenInt x: tokens) = [(Prop x,tokens)]
parseForm (TokenNeg : tokens) =
  [ (Neg f, rest) | (f,rest) <- parseForm tokens ]
parseForm (TokenCnj : TokenOP : tokens) =
  [ (Cnj fs, rest) | (fs,rest) <- parseForms tokens ]
parseForm (TokenDsj : TokenOP : tokens) =
  [ (Dsj fs, rest) | (fs,rest) <- parseForms tokens ]
parseForm (TokenOP : tokens) =
  [ (Impl f1 f2, rest) | (f1,ys) <- parseForm tokens,
                         (f2,rest) <- parseImpl ys ]
   ++
  [ (Equiv f1 f2, rest) | (f1,ys) <- parseForm tokens,
                          (f2,rest) <- parseEquiv ys ]
parseForm tokens = []

parseForms :: Parser Token [Form]
parseForms (TokenCP : tokens) = succeed [] tokens
parseForms tokens =
   [(f:fs, rest) | (f,ys) <- parseForm tokens,
                   (fs,rest) <- parseForms ys ]

parseImpl :: Parser Token Form
parseImpl (TokenImpl : tokens) =
  [ (f,ys) | (f,y:ys) <- parseForm tokens, y == TokenCP ]
parseImpl tokens = []

parseEquiv :: Parser Token Form
parseEquiv (TokenEquiv : tokens) =
  [ (f,ys) | (f,y:ys) <- parseForm tokens, y == TokenCP ]
parseEquiv tokens = []

parse :: String -> [Form]
parse s = [ f | (f,_) <- parseForm (lexer s) ]

arrowfree :: Form -> Form
arrowfree (Prop x) = Prop x
arrowfree (Neg f) = Neg (arrowfree f)
arrowfree (Cnj fs) = Cnj (map arrowfree fs)
arrowfree (Dsj fs) = Dsj (map arrowfree fs)
arrowfree (Impl f1 f2) =
  Dsj [Neg (arrowfree f1), arrowfree f2]
arrowfree (Equiv f1 f2) =
  Dsj [Cnj [f1', f2'], Cnj [Neg f1', Neg f2']]
  where f1' = arrowfree f1
        f2' = arrowfree f2

nnf :: Form -> Form
nnf (Prop x) = Prop x
nnf (Neg (Prop x)) = Neg (Prop x)
nnf (Neg (Neg f)) = nnf f
nnf (Cnj fs) = Cnj (map nnf fs)
nnf (Dsj fs) = Dsj (map nnf fs)
nnf (Neg (Cnj fs)) = Dsj (map (nnf.Neg) fs)
nnf (Neg (Dsj fs)) = Cnj (map (nnf.Neg) fs)

satisfiable :: Form -> Bool
satisfiable f = any (\ v -> evl v f) (allVals f)

-- Exercise 1

eFormF = (Cnj [(Neg (Prop 1)),(Prop 1)])
eFormT = (Dsj [(Neg (Prop 1)),(Prop 1)])

contradiction :: Form -> Bool
contradiction = not . satisfiable

-- contradiction eFormF
-- True. This is indeed a contradiction

-- contradiction eFormT
-- False. It is indeed not a contradiction

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- tautology eFormT
-- True. This is indeed an tautology

-- tautology eFormT
-- False. This is indeed an tautology

entails :: Form -> Form -> Bool
entails g f = not gt || ft
              where ft = tautology f
                    gt = tautology g

-- entails eFormF eFormF
-- False |= False = True. This is indeed a property of entails

-- entails eFormT eFormT
-- True |= True = True. This is indeed a property of entails

-- entails eFormT eFormF
-- True |= False = False. This is indeed a property of entails

-- entails eFormF eFormT
-- False |= True = True. This is indeed a property of entails

-- These defenitions are correct

equiv :: Form -> Form -> Bool
equiv f g = ft && gt || not ft && not gt
              where ft = tautology f
                    gt = tautology g
-- equiv eFormF eFormF
-- False <-> False = True. This is indeed a property of equivalence relation

-- equiv eFormT eFormT
-- True <-> True = True. This is indeed a property of equivalence relation

-- equiv eFormT eFormF
-- True <-> False = False. This is indeed a property of equivalence relation

-- equiv eFormF eFormT
-- False <-> True = False. This is indeed a property of equivalence relation

-- These defenitions are correct

-- Exercise 3 Time 4 hours

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
convertToCnf (Cnj [a]) = convertToCnf a
convertToCnf (Cnj (a:b:cs)) | length cs == 0 = cnjAB
                            | otherwise = convertToCnf (Cnj (cnjAB:cs))
                                      where cnjAB = cnjToCnj a b
convertToCnf (Dsj [a]) = convertToCnf a
convertToCnf (Dsj (a:b:cs)) | length cs == 0 = dsjAB
                            | otherwise = convertToCnf (Dsj (dsjAB:cs))
                                  where suba = convertToCnf a
                                        subb = convertToCnf b
                                        dsjAB = dsjToCnj suba subb
convertToCnf (Impl a b) = convertToCnf (Dsj [Neg (convertToCnf a), (convertToCnf b)])
convertToCnf (Equiv a b) = convertToCnf (Dsj [Cnj [da,db], Cnj [Neg da, Neg db]])
                              where da = convertToCnf a
                                    db = convertToCnf b


-- Tests:
form31 = (Impl (Prop 1) (Neg (Dsj [Prop 1, Prop 2, Prop 3])))
-- (1==>-+(1 2 3))

-- convertToCnf form31
-- *(+(-1 -3) +(-1 -1) +(-1 -2))

form32 = (Cnj [Prop 1, Prop 4, Cnj [Prop 2, Prop 3]])
-- *(1 4 *(2 3))

-- convertToCnf form32
-- *(1 4 2 3)

form33 = (Dsj [Cnj [Prop 1, Prop 2], Cnj [Prop 3, Prop 4]])
-- +(*(1 2) *(3 4))

-- convertToCnf form33
-- *(+(1 3) +(1 4) +(2 3) +(2 4))

-- Exercise 4

form41 = (Dsj [Cnj [Prop 1, Dsj [Prop 1, Prop 2, Cnj [Prop 1, Prop 2, Neg (Prop 1)]]], Cnj [Prop 3, Prop 4]])

isArrowFree :: Form -> Bool
isArrowFree f = f == arrowfree f

-- Properties: arrowfree. The form cannot contain arrow
-- isArrowFree (convertToCnf form41)
-- True

onlyNegAndNoneNegProperties :: Form -> Bool
onlyNegAndNoneNegProperties (Prop _) = True
onlyNegAndNoneNegProperties (Neg (Prop a)) = True
onlyNegAndNoneNegProperties (Neg _) = False
onlyNegAndNoneNegProperties (Cnj fs) = all onlyNegAndNoneNegProperties fs
onlyNegAndNoneNegProperties (Dsj fs) = all onlyNegAndNoneNegProperties fs
onlyNegAndNoneNegProperties (Impl fs gs) = onlyNegAndNoneNegProperties fs && onlyNegAndNoneNegProperties gs
onlyNegAndNoneNegProperties (Equiv fs gs) = onlyNegAndNoneNegProperties fs && onlyNegAndNoneNegProperties gs

-- All the negs can only contain properties
-- onlyNegAndNoneNegProperties (convertToCnf form41)
-- True

isSimpleProperty :: Form -> Bool
isSimpleProperty (Prop _) = True
isSimpleProperty (Neg (Prop a)) = True
isSimpleProperty (Dsj fs) = all isSimpleProperty fs
isSimpleProperty (Neg _) = False
isSimpleProperty (Cnj fs) = False
isSimpleProperty (Impl fs gs) = False
isSimpleProperty (Equiv fs gs) = False

andOnlyHasSimpleProperties :: Form -> Bool
andOnlyHasSimpleProperties (Prop _) = True
andOnlyHasSimpleProperties (Neg (Prop a)) = True
andOnlyHasSimpleProperties (Neg _) = False
andOnlyHasSimpleProperties (Cnj fs) = all isSimpleProperty fs
andOnlyHasSimpleProperties (Dsj fs) = all andOnlyHasSimpleProperties fs
andOnlyHasSimpleProperties (Impl fs gs) = False
andOnlyHasSimpleProperties (Equiv fs gs) = False

form42 = Cnj [Prop 1, Cnj [Prop 1, Cnj [Prop 1, Prop 1]]]
form43 = Cnj [(Prop 1),(Cnj [Prop 1, Cnj [Prop 1, (Cnj [Prop 1, (Dsj [Prop 2, Prop 3])])]])]
form44 = Impl (Cnj [Prop 1, Prop 2]) (Dsj [Prop 2, Prop 3])
form45 = Equiv (Cnj [Prop 1, Prop 2]) (Dsj [Prop 2, Prop 3])

-- All the cnj can only contain disjunctions. And all the disjunctions can only contain properties and negation for properties
-- andOnlyHasSimpleProperties (convertToCnf form41)
-- True

-- andOnlyHasSimpleProperties (convertToCnf form42)
-- True

-- andOnlyHasSimpleProperties (convertToCnf form43)
-- True

-- andOnlyHasSimpleProperties (convertToCnf form44)
-- True

-- andOnlyHasSimpleProperties (convertToCnf form45)
-- True

-- Has the same truth table as the origional
getTable :: Form -> [Bool]
getTable f = map (\ v -> evl v f) (allVals f)

hasNameTruthTable :: Form -> Form -> Bool
hasNameTruthTable f g = all id (zipWith (\x y -> x == y) (getTable f) (getTable g))

-- hasNameTruthTable form41 (convertToCnf form41)
-- True

-- hasNameTruthTable form42 (convertToCnf form42)
-- True

-- hasNameTruthTable form43 (convertToCnf form43)
-- True

-- TO test, use hasNameTruthTable, andOnlyHasSimpleProperties, onlyNegAndNoneNegProperties, isArrowFree


-- http://www.cse.chalmers.se/~rjmh/QuickCheck/manual_body.html
{-
tree = sized tree'
tree' 0 = liftM Leaf arbitrary
tree' n | n>0 =
	oneof [liftM Leaf arbitrary,
	       liftM2 Branch subtree subtree]
  where subtree = tree' (n `div` 2)
-}
-- liftM promotes to a monad
formGenerator :: Int -> Gen Form
formGenerator 0 = liftM Prop arbitrary
formGenerator n | n>0 = oneof [liftM Prop arbitrary,
                               liftM Neg subItems,
                               liftM Cnj (vectorOf 4 subItems),
                               liftM Dsj (vectorOf 4 subItems),
                               liftM2 Impl subItems subItems,
                               liftM2 Equiv subItems subItems]
                          where subItems = formGenerator (n `div` 2)

doCheck :: Form -> Bool
doCheck f = hasNameTruthTable f convertedForm &&
            andOnlyHasSimpleProperties convertedForm &&
            onlyNegAndNoneNegProperties convertedForm &&
            isArrowFree convertedForm
              where convertedForm = (convertToCnf f)

main = quickCheck $ forAll (sized formGenerator) (\x -> doCheck x)
