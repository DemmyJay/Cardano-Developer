module CCvalidator where

 ccValidator :: [Int] -> Bool 
 ccValidator cc =  (mod ((sum' (subtract9 (doubleEachOdd (reverse' (dropLast cc))))) + (checkDigit cc)) 10) == 0

 checkDigit :: [Int] -> Int
 checkDigit [x] = x
 checkDigit [] = 0
 checkDigit (x:xs) = checkDigit xs

 dropLast :: [Int] -> [Int] 
 dropLast [x]    = []
 dropLast [] = []
 dropLast (x:xs) = x : dropLast xs  

 reverse' :: [Int] -> [Int]
 reverse' []     = []
 reverse' (x:xs) = reverse' xs ++ [x]

 doubleEachOdd :: [Int] -> [Int]
 doubleEachOdd []       = []
 doubleEachOdd [x]      = [x*2]
 doubleEachOdd (x:y:zs) = x*2:y: doubleEachOdd zs 

 subtract9 :: [Int] -> [Int]
 subtract9 [] = [] 
 subtract9 (x:xs)
  | x > 9       = x-9 : subtract9 xs
  | otherwise   = x   : subtract9 xs

 sum' :: [Int] -> Int
 sum' [] = 0
 sum' (x:xs) = x + sum' xs

 cc    = [6,0,3,3,9,5,2,0,5,6,3,3,4,1,6,9] :: [Int]
 c2    = [4,5,5,6,7,3,7,5,8,6,8,9,9,8,5,5] :: [Int]
 cd    = checkDigit cc
 step1 = dropLast cc
 step2 = reverse' step1
 step3 = doubleEachOdd step2
 step4 = subtract9 step3
 step5 = sum' step4
 step6 = step5 + cd
 step7 = (mod step6 10) == 0

 {-
 init
 last
 head
 tail
 take
 map
 filter
 zip
 takeWhile
 !!
 elem
 -}

 