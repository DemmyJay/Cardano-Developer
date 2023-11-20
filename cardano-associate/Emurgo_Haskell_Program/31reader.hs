{-# LANGUAGE InstanceSigs #-}

import Control.Monad
import System.Environment
import Data.Monoid
import Data.Semigroup

type Value = Float
type Ident = String
type Table = [(Ident, Value)]
data Expr
  = Lit Value
  | Con Ident
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Seq [Expr]
  deriving Show

-- The name "Environment" is used in other not to conflict with the reader Monad.
newtype Environment e a = Environment { runEnvironment :: e -> a }

instance Functor (Environment e) where
  fmap = liftM

instance Applicative (Environment e) where
  pure  = return
  (<*>) = ap

instance Monad (Environment e) where
  return :: a -> Environment e a
  return a = Environment $ \e -> a

  (>>=) :: Environment e a -> (a -> Environment e b) -> Environment e b
  ea >>= k = Environment $ \e ->
    let a = runEnvironment ea e in runEnvironment (k a) e

ask :: Environment e e
ask = Environment $ \e -> e

consult :: String -> String -> Environment Table Value
consult x s = do
  t <- ask
  case lookup x t of
    Just v  -> return v
    Nothing -> error s

evalReader :: Expr -> Environment Table Value
evalReader (Lit v)        = return v
evalReader (Con x)        = consult x $ concat [ "Looks like ", x, " is not available!" ]
evalReader (Add e f)      = do v <- evalReader e; w <- evalReader f; return (v + w)
evalReader (Sub e f)      = do v <- evalReader e; w <- evalReader f; return (v - w)
evalReader (Mul e f)      = do v <- evalReader e; w <- evalReader f; return (v * w)
evalReader (Div e f)      = do v <- evalReader e; w <- evalReader f; return (v / w)
evalReader (Seq [e])      = evalReader e
evalReader (Seq (e : es)) = do _ <- evalReader e; evalReader (Seq es)

newtype Logger w a = Logger { runLogger :: (a, w) }

instance Monoid w => Functor (Logger w) where
  fmap :: (a -> b) -> Logger w a -> Logger w b
  fmap h la = let (a, w) = runLogger la in Logger (h a, w)

instance Monoid w => Applicative (Logger w) where
  pure  = return
  (<*>) = ap

instance Monoid w => Monad (Logger w) where
  return :: a -> Logger w a
  return a = Logger $ (a, mempty)

  (>>=) :: Logger w a -> (a -> Logger w b) -> Logger w b
  la >>= k = Logger $ let (a, w1) = runLogger la; (b, w2) = runLogger (k a) in (b, w1 <> w2)

tell :: Monoid w => w -> Logger w ()
tell w = Logger ((), w)

example :: Logger (Min Int) String 
example = do
  tell 3
  tell 4
  tell 1
  tell 7
  return "goodbye!"
