import Data.Maybe
import Control.Monad.State

type Value = Float
type Ident = String
type Table = [(Ident, Value)]
data Expr
  = Lit Value
  | Var Ident
  | Set Ident Value
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Seq [Expr]
  deriving Show

e :: Expr
e = Seq
  [
    Set "x" 7,
    Set "y" 17,
    Set "pi" 3.14,
    Mul (Var "pi") (Mul (Var "x") (Var "x"))
  ]


{- what we are doing here is taking the arguments of the constructors of "Expr", running some computation on them,
 and return the final result in the final result in the context of the State Monad -}

eval :: Expr -> State Table Value
eval (Lit v)        = return v
eval (Var x)        = do
  t <- get
  case lookup x t of
    Just v  -> return v
    Nothing -> error . concat $ ["Looks like ", x, " is not in scope!"]

eval (Set x v)  = do modify ((x, v) :); return v

-- note:

 {- "modify" takes in the partial function "(x, v):" to change the state and returns the inferred state transformer
 with void "()" as the contextual value 
 -}
{- An alternative return solution to the "put" function is: { s <- get; state $ \s -> (v, (x, v):s}
"state" is used in other to return "v" as the contextual value of the (x, v) 
-}

eval (Add e f)      = do v <- eval e; w <- eval f; return (v + w);
eval (Sub e f)      = do v <- eval e; w <- eval f; return (v - w);
eval (Mul e f)      = do v <- eval e; w <- eval f; return (v * w);
eval (Div e f)      = do v <- eval e; w <- eval f; return (v / w);
eval (Seq [])       = error "Looks like there are no expressions to evaluate."
eval (Seq [e])      = do v <- eval e; return v;
eval (Seq (e : es)) = do _ <- eval e; w <- eval (Seq es); return w

-- HOMEWORK: Try to implement `eval :: Expr -> Reader Table Value`.
-- HOMEWORK: Try to implement `eval :: Expr -> Writer String Value`.


-- Note:

{- Declaring a state Monad: "(instance Monad State s where") where "State s" is the monad and not "State", 
where "s" a polymorphic state argument which is also one of possible state monadic values-}
-- A type synonym can not be made a Monad

 {-
 1. State Monad
State Monad is just
newtype State s a = State { runState :: s -> (a, s) }
State s is the monad.

2. Monad Transformer
 A monad transformer is any type constructor which, like StateT s, takes a monad and transforms it into something more capable. 
 Other examples are ReaderT or MaybeT.

3. State Transformer
State Transformer is just a monad transformer which takes a monad m and infuses it with state manipulation capabilities
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
StateT s is the monad transformer.

-}









