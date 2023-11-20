module DataTypes where

 x :: Int
 x = 10


 fun :: Int -> Int
 fun x = x * 3


 -- Custom Data Types
 data Mood = Sad | Happy | Angry deriving Show

 data Btree = Leaf Int | Node Int Btree Btree deriving Show

 data Student = Student String Int Float deriving Show

 -- CDT Structure Notation
 data StSt = Stst {name :: String
                  , age :: Int
                  , grade :: Float} deriving Show

 -- Sysnonyms
 type PBHash = Int
 type CBHash = Int

 data Hashcash = Genesis CBHash | Block CBHash Blockchain deriving Show 

 type Blockchain = Hashcash

 gen = Genesis 0
  
 
 firstSt  = gen
 secondSt = Block 1 firstSt 
 thirdSt  = Block 2 secondSt
 fourtSt  = Block 3 thirdSt
 fifthSt  = Block 4 (Block 3 (Block 2 (Block 1 (Genesis 0))))


 --data Block = Block Int Int Float   -- It fails. Why?
  

 y = Sad
 mymood = Happy
 fun2 :: Mood -> Bool
 fun2 si = True
 
 myTree :: Btree
 myTree = Node 11 (Leaf 21) (Node 12 (Leaf 22) (Leaf 23) )
 
 theStudent = Student "Roberto" 48 80.0
 takeName :: Student -> String
 takeName (Student x y z) =  x

 the2nd = Stst {name = "Roberto", age = 48, grade =80.00}

 name2nd = name the2nd

 
