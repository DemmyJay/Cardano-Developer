 module DefiningFunctions where

 -- Conditional Expressions
 -- As in most programming languages, functions can be defined using conditional expressions. e.g 
-- If, then, else statements:
 abs :: Int -> Int
 abs n = if n >= 0 then n else -n
-- Conditional expressions can also be nested:
 signum :: Int -> Int
 signum n = if n < 0 then -1 else 
            if n == 0 then 0 else 1
-- In Haskell, conditional expressions must always have an else branch, which avoids any possible ambiguity problems with nested conditionals.

-- As an alternative to conditions, functions can also be defined using guarded equations.
 abc :: (Ord a, Num a) => a -> a
 abc n
  | n >= 0    = n 
  | otherwise = -n
-- Gaurded equation can be used to make definitions involving multiple conditions easier to read:
 signum2 n
           | n < 0     = -1
           | n == 0    = 0
           | otherwise = 1


-- Pattern Matching:

-- Many functions have a particularly clear definition using pattern matching on their arguments
 not :: Bool -> Bool
 not False = True
 not True  = False

-- Functions can often be defined in many different ways using pattern matching. e.g
 and' :: Bool -> Bool -> Bool
 True  `and'` False  = False
 True  `and'` True   = True
 False `and'` True   = False
 False `and'` False  = False
-- this above can be defined more compactly by:
-- (True && True    = True)
-- (_    && _       = False)

-- However the following definition is more sufficient,  because it avoids evaluating the second argument if the argument is False:

--True  && b = b
--False && _ = False

-- NOTE:
-- The underscore symbol (_) is a wildcard pattern that matches any argument value.
-- Patterns are matched in order. For example, the following definition always returns false:
-- and' :: Bool -> Bool -> Bool
-- _    `and'` _     = False
-- True `and'` True  = True

 

-- Patterns may not repeat variable. For example, the following definition gives an error.
-- b && b = b
-- _ && _ = False


------ List Patterns ------

-- Internally, every non- empty list is constructed by repeated use of an operator (:) called "cons" that adds an element to the start of a list. i.e
-- [1,2,3,4] means 1:(2:(3:(4:[])))

-- Functions on lists can be defined using x:xs Patterns. i.e
-- head :: [a] -> a
-- head (x:_) = x

-- tail :: [a] -> [a]
-- tail (_:xs) = xs

-- Note:
-- Always put the lists pattern expression in parenthesis

{-
------ Lambda Expressions ------

 Functions can be constructed without naming the function by using lambda expressions. i.e \x -> x + x

 Note:
-- "\" symbolizes the lambda function in Haskell.
-- in mathematics, lambda is denoted using the " |-> " as in: x |-> x + x

-- Lambda expressions can be used to give meaning to functions defined using curring.
-- For example:
 add :: Int -> Int -> Int
 add x y = x + y

-- Means:
 add1 :: Int -> (Int -> Int)
 add1 = \x -> (\y -> x + y)

-- Lambda expressions can be used to avoid naming functions that are only referenced once. e.g
 odds n = map f [0..n-1] ->
          where 
          f x = x*2 + 1
-- can be simplified as:
 odds1 n = map (\x -> x*2 + 2) [0..n-1]
-}

-- Operator Sections
-- An Operator written between it's two arguments can be converted into a curried function written before it's two arguments by using parentheses. i.e
-- 1 + 2, can also be written as (+) 1 2 would both result to 3.
-- This conversion also allows one of the arguments of the operator to be included in the parentheses. e.g
-- (1+) 2 = 3
-- Or
-- (+2) 1 = 3

-- (1+)
-- (1/)
-- (*2)
-- (/2)

-- Execises:
 safetail :: [a] -> [a]
 safetail xs = if null xs
               then [] 
               else  tail xs

 safetail2 :: [a] -> [a]
 safetail2 xs
  | null xs   = []
  | otherwise = tail xs

 safetail3 :: [a] -> [a]
 safetail3 []     = []
 safetail3 (_:xs) = xs
                  
-- Exercise 2

 or' :: Bool -> Bool -> Bool
 False `or'` False = False
 _     `or'` _     = True

 or'' :: Bool -> Bool -> Bool
 False `or''` a = a
 True  `or''` _ = True 

-- Exercise 3:

 (&&>) :: Bool -> Bool -> Bool
 (&&>) x y = if x then (if y then True else False) else False -- i.e (&&>) x y = x && y


----------------------------------- SOME BASIC HASKELL FUNCTIONS ----------------------------------

{-

head takes a list and returns its head. The head of a list is basically its first element.
head [5,4,3,2,1] = 5
   
tail takes a list and returns its tail. In other words, it chops off a list's head.
tail [5,4,3,2,1] = [4,3,2,1]

last takes a list and returns its last element.
last [5,4,3,2,1] = 1

init takes a list and returns everything except its last element.
init [5,4,3,2,1] = [5,4,3,2]

-- But what happens if we try to get the head of an empty list?
head [] would return;
*** Exception: Prelude.head: empty list.

length takes a list and returns its length, obviously.
length [5,4,3,2,1] = 5

null checks if a list is empty. If it is, it returns True, otherwise it returns False. Use this function instead of xs == [] (if you have a list called xs).
null [1,2,3] = False
null [] = True

reverse reverses a list.
reverse [5,4,3,2,1]  
[1,2,3,4,5]  
take takes a number and a list. It extracts that many elements from the beginning of the list. Watch.

take 3 [5,4,3,2,1] = [5,4,3]  
take 1 [3,9,3] = [3]  
take 5 [1,2] = [1,2]  
take 0 [6,6,6] = []  
See how if we try to take more elements than there are in the list, it just returns the list. If we try to take 0 elements, we get an empty list.

drop works in a similar way, only it drops the number of elements from the beginning of a list.
drop 3 [8,4,2,1,5,6] = [1,5,6]
drop 0 [1,2,3,4] = [1,2,3,4]
drop 100 [1,2,3,4] = [] 

maximum takes a list of stuff that can be put in some kind of order and returns the biggest element.
minimum returns the smallest.
minimum [8,4,2,1,5,6] = 1
maximum [1,9,2,3,4] = 9

sum takes a list of numbers and returns their sum.
product takes a list of numbers and returns their product.
sum [5,2,1,6,3,2,5,7] = 31  
product [6,2,1,2] = 24  
product [1,2,5,6,7,9,2,0] = 0

elem takes a thing and a list of things and tells us if that thing is an element of the list. It's usually called as an infix function because it's easier to read that way.
4 `elem` [3,4,5,6] = True
10 `elem` [3,4,5,6] = False

-}




















