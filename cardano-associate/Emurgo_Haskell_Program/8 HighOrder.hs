module HighOrder where

 add :: Int -> Int -> Int
 add x y = x + y

 add' :: Int -> Int -> Int
 add' x = \y -> x + y

 twice :: (a -> a) -> a -> a
 twice f x = f (f x)

 doubleEach :: [Int] -> [Int]
 doubleEach []       = []
 doubleEach (x:zs) = x*2:doubleEach zs 

 map' :: (a -> b) -> [a] -> [b]
 map' _ []        = []
 map' f (x:xs)    = f x : map' f xs

 filter' :: (a -> Bool) -> [a] -> [a]
 filter' _ []           = []
 filter' f (x:xs)
  | f x                 = x : filter' f xs
  | otherwise           = filter' f xs

 -- sum' :: [Int] -> Int
 -- sum' [] = 0
 -- sum' (x:xs) = x + sum' xs

 myfoldr :: (a -> b -> b) -> b -> [a] -> b
 myfoldr _ nil []           = nil 
 myfoldr f nil (x:xs)       = f x (myfoldr f nil xs)


 myfoldr' :: (a -> b -> b) -> b -> [a] -> b
 myfoldr' f nil = go
  where 
   go []          = nil
   go (x : xs)    = f x (go xs)  -- THIS IS THE KEY

 mylength = myfoldr' (\ x r -> 1 + r) 0

 sum' = myfoldr' (+) 0
 


 -- LET AND WHERE NOTATION

 aaa x = let y = x + 3
             z = x + 2
         in  y + z
 
 bbb x = y + z
  where
    y = x + 3 
    z = x + 2
