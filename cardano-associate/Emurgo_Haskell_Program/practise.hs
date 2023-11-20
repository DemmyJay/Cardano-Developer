{-# LANGUAGE InstanceSigs #-} 

module Practise where
import Control.Monad


newtype Environment e a = Environment { runEnvironment :: e -> a }


-- always implement the Functor and Applicative instances of the datatype without lift & ap 
instance Functor (Environment e) where
  fmap = liftM

instance Applicative (Environment e) where
  pure  = return
  (<*>) = ap
  -- Environment f <*> Environment a = Environment $ \e -> f (a e) -- in progress

instance Monad (Environment e) where
  return :: a -> Environment e a 
  return a = Environment (\e -> a)

  (>>=) :: Environment e a -> (a -> Environment e b) -> Environment e b
  ea >>= k = Environment $ \e -> 
            let a = (runEnvironment ea) e in (runEnvironment $ k a) e


-- To solve for e:
e :: Expr
e = Mul (Con "pi") (Mul (Lit 7) (Lit 7))
main = do
  pi : pi_value : _ <- getArgs
  print . runEnvironment (eval e)  $ [(pi, read pi_value)]

]

{- In the main function:
main = do
  pi : pi_value : _ <- getArgs
  print . runEnvironment (eval e)  $ [(pi, read pi_value)]

Internally, when we run the program, what happens is:
 print .  \e  ->  Mul $ (eval $ Con "pi" )  (eval $ Mul (Lit v)  (Lit w))  $ [(pi, read pi_value) ]

So (eval & Con "pi") looks into the list provided which is lambda argument "\e".
-}

check :: (e -> Maybe a) -> String -> Environment e a
check f s = do
  t <- ask
  case f t of
    Just v  -> return v
    Nothing -> error s  


eval (Con x) = check (lookup x) (concat ["Looks like ", x, " is not available"])












