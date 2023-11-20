module TicTacToe where

-- We need to handle
-- Player (X and O)
-- Grid and Grid State
-- Game State

--data Player = PlayerX | PlayerO deriving (Show,Eq)

data CellState= EmptyCell | AnX | AnO deriving (Show,Eq)

data Cell = Cell {xPos::Int, yPos::Int, cellState::CellState} deriving Show

type GridState = [Cell]

type GridStateTranformer = GridState -> GridState

data GameState = Running | Xwins | Owins | Atie deriving (Show,Eq)

size :: Int
size = 3


-- INITIAL GRID STATE
-- Traverse the list for 9 values (3x3) from x=1 to 3 and y=1 to 3
initialGridState :: Int -> GridState 
initialGridState gridsize = go gridsize gridsize
  where
    go 1 1 = [(Cell 1 1 EmptyCell)]
    go 1 y  = go gridsize (y-1) ++ [(Cell 1 y EmptyCell)]  
    go x y = (go (x-1) y) ++ [(Cell x y EmptyCell)]
    
iGS2 :: Int -> GridState
iGS2 gridsize = let go _ 0 = []
                    go 0 y  = go gridsize (y-1)
                    go x y = (go (x-1) y) ++ [(Cell x y EmptyCell)]
                in go gridsize gridsize

iGS3 :: Int -> GridState
iGS3 gridsize= [ (Cell x y EmptyCell) | x <- [1..gridsize], y <- [1..gridsize] ]


iGS4 :: GridState
iGS4 =  map initialValue [0..size*size-1]
  where
   initialValue :: Int -> Cell
   initialValue v = Cell (div v size + 1) (rem v size + 1) EmptyCell


-- CHANGING GRID STATE

-- Cell x y AnO
--                    ... -> State -> State 

-- MakeMove :  Validate the move, ifValid, make the gridStateChange

--MakeMove :: 
 
--errorM = if validateMove move gs then makeMove else throwError

validateBounderies :: Cell -> Int -> Bool
validateBounderies (Cell x y cs) gridSize = bounderies x && bounderies y
 where 
  bounderies n = n > 0 && n <= gridSize         -- if n > 0 && n <= size then True else False



validateMove :: Cell -> GridState -> Bool
validateMove (Cell x y cs) gs =  (f gs) == 1   -- if (f gs) == 1 then True else False
 where 
    validatePos e = xPos e == x && yPos e == y && cellState e == EmptyCell  
    f = length . filter (validatePos)
    
-- changeCell :: Cell -> GridState -> GridState
-- changeCell (Cell x y cs) gs = map (\x y cs -> filter) gs 

-- makeMove (Cell x y cs) gs = map (\el -> if (el.x == x and el.y== y) Cell x y Value else elem) cs

