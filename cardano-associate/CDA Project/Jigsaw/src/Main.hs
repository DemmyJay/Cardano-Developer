module Main where

main :: IO ()
main = putStrLn "Hello, Demmy!"

type Moves     = Int
type Size      = (Int, Int)
newtype Grid   = Grid {gridState :: [Piece]} deriving Show
data Shapes    = ULE | UME | URE | MLE | C | MRE | LLE | LME | LRE deriving Show
data Colour    = Red | Blue | Green deriving Show
data Status    = ON | Over deriving Show
data GameState = GameState
 { grid   :: Grid
 , status :: Status
 , size   :: Size
 } deriving Show

-- []

data Piece = Piece
 { shape  :: Shapes
   colour :: Colour
 } deriving Show

-- Thought process:
{- 
 Understand how the game is played.
 Correctly define each piece based on their attributes.
 Define imperatively every the game process from start to finish.
 Define every game function logic and checks within functions
-}

-- Functions


-- Abbreviations:
{-
 ULE :: Upper Left Edge 
 UME :: Upper Middle Edge
 URE :: Upper Right Edge
 MLE :: Middle Lower Edge
 C   :: Centre
 MRE :: Middle Right Edge
 LLE :: Lower Left Edge
 LME :: Lower Middle Edge
 LRE :: Lower Right Edge
-}












import Data.List

-- Define the puzzle pieces
data Piece = Piece { shape :: String, color :: String } deriving (Eq, Show)

-- Create a list of puzzle pieces
pieces :: [Piece]
pieces = [Piece "square" "red", Piece "circle" "blue", Piece "triangle" "green", Piece "rectangle" "yellow"]

-- Shuffle the pieces randomly
shufflePieces :: [Piece] -> [Piece]
shufflePieces = shuffle

-- Function to check if a puzzle is solved
isSolved :: [Piece] -> Bool
isSolved p = sort p == sort pieces

-- The main game loop
play :: [Piece] -> IO ()
play p = do
  putStrLn "Welcome to the jigsaw game!"
  putStrLn "Here are the pieces:"
  mapM_ print p
  putStrLn "Try to arrange them in the correct order!"
  putStrLn "Enter the number of the piece you want to move:"
  input <- getLine
  let i = read input :: Int
  putStrLn "Enter the number of the position you want to move it to:"
  input' <- getLine
  let j = read input' :: Int
  let newP = swap i j p
  putStrLn "Here is the new puzzle:"
  mapM_ print newP
  if (isSolved newP)
    then putStrLn "Congratulations! You solved the puzzle!"
    else play newP

main :: IO ()
main = play (shufflePieces pieces)




