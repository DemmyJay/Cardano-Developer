module MoreDataTypes where

 type Pos = (Int,Int)

 type StateChange = Pos -> Pos

 data Move = North | South | East | West

 move :: Move -> StateChange
 move North (x,y) = (x,y+1)
 move South (x,y) = (x,y-1)
 move East (x,y)  = (x+1,y)
 move West (x,y)  = (x-1,y)

 data MoreCmove = Nort Pos | Sout Pos | Eas Pos | Wes Pos

 data MoveChange = Nor StateChange | Sou StateChange

 validMovement :: Pos -> Bool
 validMovement (a,b) 
  |  a < 0      = False
  |  b < 0      = False
  | otherwise   = True



 
 