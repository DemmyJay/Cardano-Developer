module Haskell where


-- Haskell comes with Large number of standard Library functions including:
-- example of a Lists: [1,2,3,4,5,6].

-- List Operation include:
-- produce the first element: head [1,2,3,4,5] would result to [1]
-- produce all elements but the 1st one: tail [1,2,3,4,5] would result to [2,3,4,5]
-- selecting the nth element using it's position "x": [a] !! x
-- selecting the 1st n elements: take n [a]
-- Remove the first n elements from a list: drop 3 [1,2,3,4,5] would result to [4,5]
-- calculate the length of a list: length [1,2,3,4,5] would result to "5"
-- calculate the sum of a list of numbers: sum [1,2,3,4,5] would result to "15"
-- Calculate the product of a list of numbers: product [1,2,3,4,5] would result to "120"
-- Append or joining lists together: [1,2,3] ++ [4,5] would result to "[1,2,3,4,5]"
-- reverse a list: reverse [1,2,3,4,5] would result to [5,4,3,2,1]


        --functional Application
-- In mathematics functional application is denoted using parentheses and multiplication is often denoted using juxta position or space.

--f(a,b) + c d
-- Applying the function f to a & b, and add the result to the product of c & d.
-- In Haskell syntax:
-- f a b + c*d

-- Function application is assumed to have higher priority than all other operators ie;
-- f a + b means (f a) + b, rather than f(a+b).
-- How Haskell is written in Practice (Haskell Scripts):
-- As well as the function in the standard library you can also define your own functions.
-- New functions are defined within a script, a text comprising a sequence of definitions.
-- By convention, Haskell scripts usually have a .hs suffix on their filename. This is not mandatory, but is useful for identification purpose.

-- My first script:
-- starting GHCi: $ ghci 3firstStep.hs.
-- once the standard library and the "3firstStep.hs" file are loaded, the functions from both can be used. 
double :: Num a => a -> a
double x    = x + x
--run: "double 10" would result to 20
quadruple x = double (double x)
-- run: "quadruple 10" would result to 40
factorial n = product [1..n]
average ns  = sum ns `div` length ns
-- length ns (div is enclosed in back quotes, not forward)
-- x `f` y is just syntactic sugar for f x y.

-- sample code:
f = 4
g = 3:4:5:4:[]
add :: a -> [a] -> [a]
add x ys     = x:ys

-- Know GHCi Commands!
-- Function and argument names are written in lower cases
-- By Convention, list arguments have an "s" suffix on their names i.e xs, ns, nss

-- Obey the layout rule;
-- The layout rule avoids the need for explicit syntax to indicate the grouping of definitions.

-- Exercises
-- corrected syntax:
n = a `div` length xs
    where
    a  = 10
    xs = [1,2,3,4,5]

-- show library function "last" 
last' :: [a] -> a
last' [] = error "empty list"
last' xs = head (reverse xs)

-- Also:
last'' :: [a] -> a
last'' xs = xs !! n
           where
           n = length xs - 1
-- show how the function "init" that removes the last element from a list can be defined in two different ways.

-- 1st answer:
inita :: [a] -> [a]
inita zs = reverse (init' zs)
            where
            init' zs = drop 1 (reverse zs)

--2nd answer:
initb :: [a] -> [a]
initb ys = take (length ys - 1) ys























--
















