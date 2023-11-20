
 module ListComprehension where



   

-- Programming with Lists.
-- Set Comprehension:
-- In mathematics, the comprehension notation can be used to construct new sets from old sets.
-- {x^2 | x E {1..5}} which means: The lists {1,4,9,16,25} of all numbers x^2 such that x is an element of the n list {1..5}

{- 
   In Haskell:
   The comprehension notation can be used to construct new lists from old lists.
   [x^2 | x <- [1..5]] which means: The lists {1,4,9,16,25} of all numbers x^2 such that x is an element of the n list [1..5]
-}

-- Difference between a set and a list:
 {- In a set, the order in which elements appear doesn't matter, and the number of times an element appears in the set doesn't 
    matter as well all that matters is that all the elements of that set are present whereas in a list, the order of elements matter 
    ie "[2,3,4,5] /= [5,4,3,2]" and the number of times the elements appear in the list matters as well.
 -}

-- Note: 
{- The expression "x <-[1..5]" is called a generator as it state how to generate values for x. -}


{-
   * Comprehensions can have multiple generators separated by commas. For example: [(x,y) | x <- [1,2,3], y <- [4,5]]
   * Changing the order of the generators changes the order of the elements . e.g in the list: [(x,y) | y <- [4,5], x <- [1,2,3]] 
     would result to a different list compared to [(x,y) | x <- [1,2,3], y <- [4,5]].
   * Multiple generators are like nested loops, with later generators as more deeply nested loops whose variable value more frequently.
-}

-- Dependent Generators:
{- 
Later generators can depend on the variables that are introduced by earlier generators. i.e [(x,y) | x <- [1..3], y <- [x..3]] == [(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)].
Using a dependent generator, we can define the library function  that concatenates a list of lists: i.e concat :: [[a]] -> [a].
concat xss = [x | xs <- xss, x <- xs]
-}

-- for example: concat [[1,2,3],[4,5],[6]] = [1,2,3,4,5,6]

-- Guards

-- List comprehensions can use restrict the values produced by earlier generators. i.e;
-- [x | x <- [1..10], even x] == [2,4,6,8,10]

-- Using a guard to map a positive integer to it's list of factors:

factors :: Int -> [Int]
factors n =
   [x | x <- [1..n], n `mod` x == 0] --(This function creates a list of all factors of "n")
-- Example:
--factors 15 = [1,3,5,15] 

-- Prime Number: A positive integer is prime if it's factors are only 1 and Itself i.e the factors of "3" are 1 & 3 so the integer "3" is a prime number.
-- In Haskell:
prime :: Int -> Bool
prime 1 = True
prime n =
   factors n == [1,n]

-- To generate prime numbers from 1 to a variable number "n":
primes :: Int -> [Int]
primes n =
   [x | x <- [1..n], prime x]

-- A faster algorithm to do this would to use the "sieve of Eratosthenes" which is an algorithm that utilizes lazy evaluation to generate all prime numbers within a defined range.
sieve :: [Int] -> [Int]
sieve [] = []
sieve (y:ys) =
       y : sieve [x | x <- ys, x `mod` y /= 0]
-- and then use the library function "takeWhile" to take all elements of the list that fall within a specific range.

allPrimes :: [Int]
allPrimes = sieve [2..]

primes' = takeWhile (<= 15000) allPrimes


-- The Zip Function ---
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y): zip' xs ys


------  Pairs ------
-- Define a pair function that takes a list and returns a list of all pairs of adjacent elements.

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs xs = zip' xs (tail xs)

-- using the pair function, one can determine if the elements of a list are in the correct order.





























