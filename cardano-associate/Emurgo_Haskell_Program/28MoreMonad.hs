import Prelude hiding (liftA2)
import Control.Monad hiding (ap)

type Value = Float
data Expr
  = Lit Value
  | Add   Expr Expr
  | Sub   Expr Expr
  | Mult  Expr Expr
  | Div   Expr Expr
  | Sqrt  Expr
  deriving Show

-- eval :: Expr -> Value
-- eval (Lit v)    = v
-- eval (Add e f)  = eval e + eval f
-- eval (Sub e f)  = eval e - eval f
-- eval (Mult e f) = eval e * eval f
-- eval (Div e f)  = eval e / eval f
-- eval (Sqrt e)   = sqrt $ eval e

expSqrt :: Float -> [Float]
expSqrt x = map ($ (sqrt x)) [ id, negate ]

ap :: [] (a -> b) -> [] a -> [] b
ap [] _         = []
ap (f : fs) xs  = fmap f xs ++ ap fs xs

evalList :: Expr -> [Value]
evalList (Lit v)    = return v
evalList (Add e f)  =
  evalList e >>= \v ->
  evalList f >>= \w ->
  return (v + w)
evalList (Sub e f)  =
  evalList e >>= \v ->
  evalList f >>= \w ->
  return (v - w)
evalList (Mult e f) =
  evalList e >>= \v ->
  evalList f >>= \w -> 
  return (v * w)
evalList (Div e f)  =
  evalList e >>= \v ->
  evalList f >>= \w -> 
  return (v / w)
evalList (Sqrt e)   =
  evalList e >>= \v ->
  expSqrt v

e = (Add
      (Lit 2)
      (Sqrt (Mult (Lit 3) (Lit 7))))

-- primes
primes :: Int -> [Int]
primes n = do
  x <- [ 1 .. n ]
  guard (factors x == [ 1, x ])
  return x where
  factors m = do
    y <- [ 1 .. m ]
    guard (mod m y == 0)
    return y
