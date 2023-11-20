
module Practise where

import Control.Exception

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys



-- elapsedTimeInMinutes, expectedMinutesInOven, preparationTimeInMinutes) where
type Minutes = Integer
type Layers = Integer

expectedMinutesInOven :: Minutes
expectedMinutesInOven = 40

preparationTimeInMinutes :: Layers -> Minutes
preparationTimeInMinutes = (* 2)

elapsedTimeInMinutes :: Layers -> Minutes -> Minutes
elapsedTimeInMinutes = (+) . preparationTimeInMinutes

-- Recreated function --
-- [a] !! x can also be coded as:
position :: [a] -> Int -> a
position [] _ = error "empty list"
position xs g = last $ take (g+1) xs

position' :: Eq a => [a] -> Int -> a
position' xs g
 | null xs = error "empty list"
 | otherwise = last $ take (g+1) xs

fold' :: (a -> a -> a) -> [a] -> a
fold' f [] = error "empty set"
fold' f [x] = x
fold' f (x:y:xs) = fold' f (f x y : xs)





