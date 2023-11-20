module Recursion where

 abc :: [Int]
 abc = [1..22]

 length' :: [a] -> Int
 length' [] = 0
 length' lst = 1 + length' (tail lst)

 length'' :: [a] -> Int
 length'' [] = 0
 length'' (x:xs) = 1 + length'' xs

 sum' :: [Int] -> Int
 sum' [] = 0
 sum' (x:xs) = x + sum' xs


 sumOf2 :: [Int] -> [Int]
 sumOf2 []       = []
 sumOf2 [x]      = [x]
 sumOf2 (x:y:zs) = (x + y) : sumOf2 zs 

 ex1 = sumOf2 [1]
 ex2 = sumOf2 [1,2]
 ex3 = sumOf2 abc

-- Concatenation operators
-- a : [a]
-- [a] ++ [a]

 add2 :: Int -> Int
 add2 n = n + 2

 
 add2toAll :: (Int -> Int) -> [Int] -> [Int]
 add2toAll _ []       = []
 add2toAll fun (x:xs) = (fun x) : add2toAll fun xs 

