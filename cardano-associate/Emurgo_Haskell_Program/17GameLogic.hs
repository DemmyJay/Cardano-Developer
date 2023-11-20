module GameLogic where
import TicTacToe (CellState, Cell, GridState, GameState, WorldState) 

 --Verify gameState
 --The tie implies NO winner and NO Empty cellState

 --Define Winning States

 
anyColWinner :: GridState -> Bool
anyColWinner = error "Define function"

anyRowWinner :: GridState -> Bool
anyRowWinner = error "Define function" 

anyDiagWinner :: GridState -> Bool
anyDiagWinner = error "Define function" 


 --Any col winner (Cell x _) 
 --Any row winner (Cell _ y)
 --Any diagonal winner (Cell x y)  x <- 1..size  y <- 1..size  |x <- size..1, y <- 1..size


