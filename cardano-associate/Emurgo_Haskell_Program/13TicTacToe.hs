module TicTacToe02 where

--1. Explain use of custom data types for Cell State, the state could be Empty, an X or an O, Player, Game and State
 data Player = PlayerX | PlayerO deriving (Eq,Show)
 data State = Running | GameOver deriving Show
 data Game = Game {gridState :: GridState
                 ,gamePlayer :: Player
                 ,gameState :: State
                 ,moves :: Int
                 } deriving Show

 data CellState = EmptyCell | AnX | AnO deriving (Eq,Show)
 
 size :: Int
 size = 3
 
--2. Some aliases to avoid repetitive typing and to include readability of outcomes

 type Cell = (Int,Int,CellState)
 type GridState = [Cell]
 
--3.1. Lists of Lists
--     Create initial Grid and GridState  

 initialGrid :: Int -> Int -> GridState
 initialGrid x y = createListInList x y
   where 
       createListInList :: Int -> Int -> GridState 
       createListInList 0 0  = []
       createListInList _ 0  = []
       createListInList 0 _  = []
       createListInList hL vL 
        | (hL-1) < 0 = [] 
        | otherwise   = if (vL-1) < 0
                         then [] 
                         else (createListInList (hL-1) vL) ++ (createInitialRow hL (vL-1)) ++ [((hL),(vL),EmptyCell)]

       createInitialRow :: Int -> Int -> GridState 
       createInitialRow _ 0 = []
       createInitialRow hL vL
        | (vL-1) < 0     = [] 
        | otherwise       = (createInitialRow hL (vL-1)) ++ [(hL,vL,EmptyCell)]

 setCellState :: Cell -> GridState -> GridState
 setCellState (h,v,cS) xs = map (\x -> if isEmptyCell h v x then (h,v,cS) else x) xs

 isEmptyCell :: Int-> Int-> Cell -> Bool
 isEmptyCell x y (h,v,cS) = x == h && y == v && cS == EmptyCell


--3.2. Single List
--     Create initial Grid and GridState  
 type GridState' = [Cell]
 initialGrid' :: Int -> GridState'
 initialGrid' n = [(x,y,cS) | x <- [1..n]
                            , y <- [1..n]
                            , cS <- [EmptyCell]
                            ]

 setCellState' :: Cell -> GridState' -> GridState'
 setCellState' (h,v,cS) pgs = map (\x -> if isEmptyCell h v x then (h,v,cS) else x) pgs 

 isEmptyCell' :: Int-> Int-> Cell -> Bool
 isEmptyCell' x y (h,v,cS) = x == h && y == v && cS == EmptyCell


 

 -- Single List Data Structure for all the items on the Grid
 createList :: Int -> CellState -> [Cell]
 createList n cS = [(x,y,cell) | x <- [1..n], y <- [1..n], cell <- [cS]]
 
-- createList' :: Int -> Int -> CellState -> [Cell]
-- createList' h v cS = [(x,y,cell) | x <- [h], y <- [1..v], cell <- [cS]]

-- 4. Change state of the GridState

-- data CellState = EmptyCell | AnX | AnO deriving (Eq,Show)
-- type Cell = (Int,Int,CellState)
-- type GridState = [[Cell]]

 changeGridState :: Cell -> GridState -> GridState
 changeGridState _ []           = error "Errors from this point forward"
 changeGridState (x,y,cS) grid 
   | x > length grid          = error "Horizontal Input out of grid"
   | otherwise                  =  if y > length (head grid)
                                   then error " Vertical Input out of grid"
                                   else setCellState (x,y,cS) grid

 -- | y > (length grid)  = error "Input out of grid" 
   
 -- changeGridState' :: Cell -> GridState -> GridSate
 -- change