module Lab4 where

import Lecture4
import SetOrd
import Data.Tuple
import Data.List
import System.Random
import Control.Monad
import Test.QuickCheck


{-  Exercise 1:     Read or reread Chapter 4 of The Haskell Road, and make
                    a list of questions on specific points that cause difficulty
                    of understanding.
    Time spent:     2 hours  -}

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
    

{-  Exercise 4:     Read or reread Chapter 5 of The Haskell Road, and make a list of questions on 
                    specific points that cause difficulty of understanding.
    Time spent:     ... hours  -}

{-  ### Chapter 5: Relations ### 
    Time spent:     2 hours and 15 minutes  -}

{-  
    1.   Why is the relation described in example 5.63 equivalent? The argument
         given generates some confusion.
    2.   We find the concept of equivalence classes and partitions in section 5.6
         a bit confusing.

-}

{- Exercise 7:  Is there a difference between 
                - the symmetric closure of the transitive closure of a relation RR 
                - and the transitive closure of the symmetric closure of RR?} 
    Time spent: 1 hour           
-}

type Rel a = [(a,a)]
                
comb :: (Eq a) => Rel a -> Rel a -> Rel a
comb [] ys = ys
comb (x:xs) ys = if elem x rest then rest else x:rest
                                where rest = comb ys xs
                
trClo :: Ord a => Rel a -> Rel a
trClo [] = []
trClo [x] = [x]
trClo (relation:relations) = [relation] ++ (trCloHelper [relation] [relations]) ++  (trClo relations)
trCloHelper x [y] = [(x1, y2) | (x1, x2) <- x, (y1, y2) <- trClo (y), x2 == y1]
                
                
symClos :: Ord a => Rel a -> Rel a
symClos xs = comb xs swaped
                    where swaped = map swap xs


{- Function to check if they are the same -}
eqClosure :: Rel Int -> Bool
eqClosure r = (symClos (trClo r)) == (trClo (symClos r))

symTransClos, transSymClos :: Rel Int -> IO()
symTransClos r = print (symClos (trClo r))
transSymClos r = print (trClo (symClos r))


{-  If we use the following input:
    [(1,2)]
    The function 'eqClosure' returns 'false' 
    
    Using the function symTransClos we get: [(1,2),(2,1)]
    Using the function transSymClos we get: [(1,2),(1,1),(2,1)]

    So we can conclude that there is in fact a difference between 
    the symmetric closure of a relation R and the transitive closure
    of the symmetric closure of R.
-}

