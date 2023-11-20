module MoreRecursion where

 factorial :: Int -> Int
 factorial 0 = 1
 factorial n = n * factorial (n-1)

 factorial' :: Int -> Int
 factorial' n 
  | n <= 0     = 1
  | otherwise  = n * factorial' (n-1)

 sum' :: [Int] -> Int
 sum' [] = 0
 sum' (x:xs) = x + sum' xs
 
 data Health = Healthy | Sick deriving Show
 data Temperature = C Int deriving (Show)

 temps :: [Temperature]
 temps = [C 36, C 37, C 38, C 40, C 35]

 areUsick :: [Temperature] -> [Health]
 areUsick []         = []
 areUsick ((C y):xs) = (if y >= 35 && y <= 36
                        then Healthy
                        else Sick)
                     : areUsick xs


 data Grades = Grade Int Char deriving Show

 grades :: [Grades]
 grades = [Grade 68 'D', Grade 78 'C', Grade 89 'B', Grade 96 'A']

 average :: [Grades] -> Int
 average []          = 0
 average lst = (sumGrades lst)  `div` (length lst)  
   where 
     sumGrades :: [Grades] -> Int
     sumGrades []                   = 0
     sumGrades ((Grade num ltr):xs) = num + sumGrades xs

 avg' :: [Grades] -> Int
 avg' []          = 0
 avg' lst@((Grade num ltr):xs) = (num `div` (llst)) + avg' xs  

 llst = length grades


 -- infix notation is dont with ` , ex. `div` 
