module TicTacToe03 where

-- We need to handle
-- Player (X and O)
-- Grid and Grid State
-- Game State

--data Player = PlayerX | PlayerO deriving (Show,Eq)

data CellState= EmptyCell | AnX | AnO deriving (Show,Eq)
data Cell = Cell {xPos::Int, yPos::Int, cellState::CellState} deriving Show
type GridState = [Cell]

data GameState = Running | Xwon | Owon | Atie deriving (Show,Eq)

data WorldState = WS {
                      player :: String,      --(String,String) works in JacoRudolp logic
                      gridState :: GridState,
                      gameState :: GameState,
                      movement :: Int
                     }

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


-- MAKE A MOVE

makeMove :: Cell -> WorldState -> WorldState
makeMove (Cell x y cs) ws = error "Need to implement function"
 where
  verifyGameState :: GridState -> GameState
  verifyGameState = error "Need to implement function"


-- If Player with X-token wins then gameState = Xwins
--                             else if player with O token wins 
--                                  then gameState = Owins  
--                                  else gameState = Atie





--verifyGameState  :: GridState -> GameState   -- State Management is taking the input State as Default "Running"
-- JacoRudolp logic
-- verifyGameState :: WorldState -> GameState
-- Sebastian logic
-- verifyGameState :: GridState -> GameState 


-- JacoRudolp logic:
--   After defining winning state, infer winner as the last player to make a move (the one in the current WorldState)

-- Sebastian logic: 
--   After defining winning state, take the cellState of the first winnint state cell and select the right player

-- CHANGING GRID STATE

changeGridState :: Cell -> GridState -> GridState
changeGridState _ []  = []
changeGridState (Cell _ _ EmptyCell) gs = error "You must play an X or an O"
changeGridState (Cell x y cs) gs
 | validateMove (Cell x y cs) gs = map (\c -> if (x == (xPos c)) && (y == (yPos c)) 
                                                    then (Cell x y cs)
                                                    else c) gs


--VALIDATIONS

validateBounderies :: Cell -> Int -> Bool
validateBounderies (Cell x y cs) gridSize = bounderies x && bounderies y
 where 
  bounderies n = n > 0 && n <= gridSize         -- if n > 0 && n <= size then True else False


validateMove :: Cell -> GridState -> Bool
validateMove (Cell x y cs) gs =  (f gs) == 1   -- if (f gs) == 1 then True else False
 where 
    validatePos e = xPos e == x && yPos e == y && cellState e == EmptyCell  
    f = length . filter (validatePos)
    



--TESTING

step1 = iGS3 size
step2 = changeGridState (Cell 1 1 AnO) step1
step3 = changeGridState (Cell 1 2 AnX) step2
step4 = changeGridState (Cell 2 2 AnO) step3
step5 = changeGridState (Cell 1 3 AnX) step4
step6 = changeGridState (Cell 3 3 AnO) step5
