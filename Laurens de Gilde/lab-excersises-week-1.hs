module Lab1 where
import Data.List
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
	    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

primesList :: [Integer] -> [Integer]
primesList ps = filter prime ps 

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
(-->)  p q = (not p) || q
 
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all


-------------------- Lab excersise 1 --------------------

---------- Workshop excersise 2 ----------


exc2Left :: Int -> Int
exc2Left n = sum [x^2 | x <- [0..n]]

exc2Right :: Int -> Int
exc2Right n = (n * (n + 1) * (2 * n + 1)) `div` 6

exc2Test :: Int -> Bool
exc2Test n | n >= 0 = exc2Left n == exc2Right n
		   | otherwise = True

exc2 = quickCheck exc2Test	

---------- Workshop excersise 3 ----------

exc3Left :: Int -> Int
exc3Left n = sum [x^3 | x <- [0..n]]

exc3Right :: Int -> Int
exc3Right n = ((n * (n + 1)) `div` 2) ^ 2


exc3Test n | n >= 0 = exc3Left n == exc3Right n
		   | otherwise = True

exc3 = quickCheck exc3Test


-------------------- Lab excersise 2 --------------------

---------- Workshop excersise 4 ----------

exc4Left :: [a] -> Int
exc4Left ns = length (subsequences ns) 

exc4Right :: [a] -> Int
exc4Right ns = 2 ^ (length ns)

exc4Test :: Int -> Bool
exc4Test n = exc4Left ns == exc4Right ns
		 where
		 	ns = [1..n]  
-- Creating this test simpulates the base case (left side of equation) and the n + 1 case(right side of equation)
exc4InductionTest n = exc4Test n == exc4Test (n+1)

-- Question: 'Is the property hard to test? If you find that it is, can you give a reason why?'
-- Answer: I find this not very hard to test. It is pretty much the same as testing for the previous 2 excersises. 
-- I created a left and right hand to simulate a equation by checking if both the values are equal. 

-- Question: 'Give your thoughts on the following issue: when you perform the test for exercise 4, what are you testing actually? Are you checking a mathematical fact? Or are you testing whether subsequences satisfies a part of its specification? Or are you testing something else still?'
-- Answer: Although I think this is not a very valuable test because in logic it is common knowledge that the size of a power set of A is 2 times the square root of the set size of A.
-- I am not testing any mathematical equation here. What I am testing is if the logical equation holds, which we already knew since it has been verified by many proffessors. 
-- On the other hand it is also a check whether the subsequences function works as it should. Since we know the logical equation holds, we can test if what subsequences returns is correct. 
-- By using the logicall equation. The only thing that we have to be aware of is the type of subsequences. For intstance if I define my own subsequences function that takes an Int and returns an Int
-- Than the my own subsequences function would mislead me in checking if the function returns the right power set of a set.
-- The last example is pretty far fetched in my opion but it came to my mind so i typed it down.

-------------------- Lab excersises 3 --------------------

---------- Workshop excersise 5 ----------

perms :: [a] ->[[a]]
perms [] = [[]]

perms (x:xs) = concat (map (insrt x) (perms xs))
			 where
			   insrt x [] = [[x]]
			   insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

-- Function credits: http://vvv.tobiassjosten.net/haskell/factorials-in-haskell/
factorial 0 = 1
factorial n = n * factorial (n - 1)
-- 

exc5Left :: [a] -> Int
exc5Left ns = length(perms ns);

exc5Right :: Int -> Int
exc5Right n =  factorial n

exc5Test :: [a] -> Bool
exc5Test ns = exc5Left ns == exc5Right (length ns)	

-- Question: 'Is the property hard to test? If you find that it is, can you give a reason why?'
-- Answer: Same as before. Creating two equations, one for the left and one for the right, and check if these values are the same. I took the factorial function of the web.

-- Question: 'Give your thoughts on the following issue: when you perform the test for exercise 4, what are you testing actually? Are you checking a mathematical fact? Or are you testing whether subsequences satisfies a part of its specification? Or are you testing something else still?'
-- Answer: I think this test is more usefull then the test before. Mainly because botht the functions that we are testing are "self made". By knowing what the output should be of the left side
-- And knowing the output of the right side, and knowing that these two SHOULD be the this test is vaidating both functions in one test. 



-------------------- Lab excersise 4 --------------------

reversal :: Integer -> Integer
reversal = read . reverse . show

checkPrimeReversal :: [Integer] -> [Integer]
checkPrimeReversal [] = []
checkPrimeReversal (p:ps) | elem (reversal p) ps = p : checkPrimeReversal ps
						  | otherwise = checkPrimeReversal ps
checkReversalPrimes (xs) = checkPrimeReversal (primesList xs)

-------------------- Lab excersise 5 --------------------

takeSmallestPrime :: Int -> Int -> [Integer] -> Integer
takeSmallestPrime x y ps | prime (sumPrime) = sumPrime
					     | otherwise = takeSmallestPrime (x+1) (y+1) (tail ps)
						 where
						  	sumPrime = sum ( drop x . take y $ ps)	
-- Question: 'Do you have to test that your answer is correct? How could this be checked?'
-- Answer: In my opinion this does not have to be checked if the answer is correct.
-- What I am doing it starting from 0 to 101 and if this is not a prime I increment
-- the 0 and 101 with 1. Meaning I take the next 101 consecutive primes sum these
-- and check if this is a prime. If not I recurse again and again.
-- This is a solid way of getting the smallest prime from a range.
-- The only thing that has to be checked is if the function prime works as I think it works.

-------------------- Lab excersise 6 --------------------



-------------------- Lab excersise 7 --------------------



-------------------- Lab excersise 8 --------------------


data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq, Show)

boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]
				  	
accuses :: Boy -> Boy -> Bool
-- Matthew accuses
accuses Matthew x | x == Carl = False
				  | x == Matthew = False
				  | otherwise = True
-- Peter accuses
accuses Peter x | x == Matthew = True
				| x == Jack = True
				| otherwise = False
-- Jack accuses
accuses Jack x = not ( (accuses Matthew x) || (accuses Peter x) )
-- Arnold accuses
accuses Arnold x = accuses Matthew x /= accuses Peter x
-- Carl accuses
accuses Carl x = not (accuses Arnold x)

accusers :: Boy -> [Boy]
accusers x = [b | b <- boys, accuses b x]

honest :: [Boy]
honest = [b | b <- boys, g <- guilty, accuses b g]

guilty :: [Boy]
guilty = [b | b <- boys, length (accusers b) == 3 ]




