{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE InstanceSigs #-}
module Typeclasses where

{-




Unparameterized Custom Data Type:

Here's how you would define a type class instance for an unparameterized custom data type:

-} 
data Temp = C Float | F Float deriving (Show)

data Blockchain =  Genesis | Block Temp Blockchain | LastBlock deriving (Show,Eq)

myblock :: Blockchain
myblock = Block (C 100) (Block (C 2) (Block (F 1) (Block (F 2) (LastBlock))))

urblock1 :: Blockchain
urblock1 = Block (F 212) (Block (C 2) (Block (F 1) (Block (F 2) (LastBlock))))

urblock2 :: Blockchain
urblock2 = Block (C 2) (Block (C 3) (Block (F 4) (Block (F 2) (LastBlock))))


x :: Temp
x = C 100

y :: Temp
y = F 212

z :: Bool
z =  x == y
 

     
instance Eq Temp where
     (==) :: Temp -> Temp -> Bool  
     (==) (C x) (C y) = x == y
     (==) (F x) (F y) = x == y
     (==) (C x) (F y) = (1.8*x +32) == y
     (==) (F x) (C y) = (1.8*y +32) == x
  

-- Parameterized Custom Data Type:

-- When you have a parameterized (polymorphic) custom data type, you include type variables in the instance declaration and add necessary constraints. 
-- Here's an example using a parameterized type with the Eq type class:

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
    (==) :: Eq a => Pair a -> Pair a -> Bool
    (Pair x y) == (Pair u v) = x == u && y == v

{-
In this case, Pair is a parameterized data type that can hold values of any type a. 
The type class instance for Eq specifies that for Pair a to be an instance of Eq, a must also be an instance of Eq. 
This is expressed in the constraint (Eq a =>) in the instance declaration. It ensures that you can compare values of type Pair a if you can compare values of type a.

When working with parameterized data types, you'll often see type class constraints on the type variables in instance declarations. 
These constraints specify what type classes the type variables must belong to in order for the instance to be valid. 
This allows you to define generic functionality that works with different types, as long as those types satisfy the required constraints.
-}