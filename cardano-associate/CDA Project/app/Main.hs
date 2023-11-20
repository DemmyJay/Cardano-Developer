module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

type Point = (Int, Int)
type Score = Int
type Size  = (Int, Int)
{- difference between newtype and data is that, while compiling,
 Haskell would automatically convert the snake value 
 into a list of point after type checking so there wouldn't be any over-head 
 while unwrapping or wrapping all those functions which are going to be optimized-}

newtype Snake  = Snake {getSnake :: [Point]}
newtype Food   = Food  {getFood  :: Point}
data Direction = UP | Down | Left | Right
data Status    = ON | Over 
data Config    =
  { girdSize :: Size
  , 
  }
data Game      = Game
  { snake     :: Snake 
  , food      :: Food
  , score     :: Score
  , direction :: Direction 
  }

data Config = Config 
 {
  screenSize :: Size
 }

{- in general, any form of operation involving indexing or
 calculation of length of a linked list should be avoided.
 If it is necessary, use another datatype like "Seq"-}




