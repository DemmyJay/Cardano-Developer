module TypesAndClasses where


 --     ______________________Emurgo Haskell Orientation Course__________________
      
      -------------- All about Types ----------------

-- Haskell is split into two levels, the Type-level and the Value-level.
-- Type-level defines the blueprint for a haskell function components in our program. i.e type signature e.g isPositive :: Integer -> Int.
-- Value-level defines the actual expression of a Haskell function, it's arguments and the body of the function as specified at the value-level. 
   
-- What is a type?
-- A type basically is a name for a collection of associated values. e.g in Haskell "Bool" is a type of logical values; True & False.

 -- Type Error: This is basically applying a function to one or more arguments of the wrong type. e.g  1 + False.

     ------------- Types in Haskell ------------

-- If evaluating an expression e would produce a value of type t, then e has type t written as; e :: t.
-- Every well formed expression has a type, which can be automatically calculated a compile time using a process called type inference.
-- All type errors are found at compile time, which makes programs safer and safer by removing the checks at run time.
-- In GHCi, the type command calculates the type of an expression, without evaluating it:
--  > not False == True.
--  > :type not False would result to not false :: Bool.

----- Basic Haskell built-in Types -----

-- Bool
-- Char
-- string
-- Int
-- Float
-- Double

------   Types can also be structured -------

-- Lists
-- Tuple
-- Functions

-- A list is a sequence of values of the same type. e.g A list of Ints would be [1,2,3,4,5]. The types of elements is unrestricted. 

-- Tuple Types:
-- A tuple is a structure that stores a group of values of different data types. e.g (False,true) :: (Bool,Bool)
-- The type of the components is unrestricted

-- The Function Type
-- A function is a mapping from values of one type to values of another type. e.g the function "not" take in a type Bool and returns a type Bool i.e not :: Bool -> Bool.
-- In general t1 -> t2 is the of function that maps the values of type t1 to type t2.

-- Note:
-- The arrow is typed at the keyboard as ->

-- Curried Functions:
-- Curried functions are functions that takes in arguments and return a function as a result. 
-- add :: Int -> (Int -> Int)
-- add x y = x + y Is curried as ((add x)y)
-- Note: Curried functions take arguments one at a time.
-- Curried conventions

-- Polymorphic function:
-- A polymorphic function is that which it's type contains one or more variable types. e.g
-- length :: [a] -> Int

-- Overloaded Functions:
-- polymorphic function is called overloaded if it's type contains one or more type constraints. e.g (+) :: Num a => a -> a -> a
-- constrained type variables can be instantiated to any type that satisfy the constraint. i.e
-- 1 + 2     -> a = Int
-- 1.0 + 2.0 -> a = Float
-- 'a' + 'b' -> Chars is not a numeric type.

          -------------------- Custom types -------------------

-- Custom Types or Variant Types can also be declared with the "data" key word.
-- They can be declared as Variant Types which consists of the type name and one or more data constructors e.g data Colour = Blue | Yellow | Red    
-- They could also be declared as Record types which are Types with one or more constructors each containing one or multiple field types e.g
data FullName = Name String String deriving Show

x :: FullName 
x = Name "Dave" "Myers"

-- Custom types can have Selectors attached to their fields. Selectors are functions that when applied to a record type, are used to return the valve of it's associated field.
data User = Admin  {getUsername :: String, getEmail :: String}
          | Editor {getUsername :: String, getEmail :: String}
          | Author {getUsername :: String, getEmail :: String} deriving Show

-- Custom Type complexity:
-- custom Types can be parameterized by a variables to hold data of any type, they can have a mix between Variant and Record types and They can also reference themselves to create a recursive data structure e.g.
data Tree a = Leaf | Node { getNodeValue   :: a,
                            getLeftBranch  :: Tree a,
                            getRightBranch :: Tree a
                          } deriving Show   -- The "Leaf" is the end of the Tree, if the Tree is a Node, it consists of a value of type "a". 
                                            -- The Node can also branch into other Tree values i.e Leaf or Node.


----------- TypeClasses ----------

-- TypeClasses provide an abstract interface defined by common behaviors called methods and occasionally values.

-- Haskell has a number of type classes, including: 

-- Num - Numeric typeClass.
-- Eq  - Equality typeClass.
-- Ord - Ordering typeClass.

------ For example ------

-- (+)  :: Num a => a -> a -> a
-- (==) :: Eq a => a -> a -> Bool
-- (<)  :: Ord a => a -> a -> Bool

-- Exercises 1
type1 :: [Char]
type1 = ['a','b','c']

type2 :: (Char,Char,Char)
type2 = ('a','b','c')

type3 :: [(Bool,Char)]
type3 = [(False,'0'),(True,'1')]

type4 :: ([Bool],[Char])
type4 = ([False,True],['0','1'])

type5 :: [[a] -> [a]]
type5 = [tail,init,reverse]

-- Exercise 2
second :: [a] -> a
second xs = head(tail xs)

swap :: (a,b) -> (b,a)  
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

  
twice :: (t -> t) -> t -> t
twice f x = f (f x)
















































