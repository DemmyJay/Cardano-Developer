module EitherMonad where
import Data.Either

data Expr = Con Int
          | Add Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving Show

ctd1 :: Expr
ctd1 = Add ( Mul (Div (Con 12) (Con 0)) (Add (Con 2) (Con 1))) (Con 2)
ctd2 = Add ( Mul (Div (Con 12) (Con 4)) (Add (Con 2) (Con 1))) (Con 2)
 
safeDiv :: Int -> Int -> Either String Int
safeDiv _ 0 = Left "You know you can't divide by zero"
safeDiv x y = Right (x `div` y)

divE = "You know you can't divide by zero" 

safeEval :: Expr -> Either String Int
safeEval (Con v) = Right v
safeEval (Add e f) = case safeEval e of
    Left "error"  -> Left "error"
    Left divE     -> Left divE 
    Right e'      -> case safeEval f of
        Left "error"  -> Left "error"
        Left divE     -> Left divE
        Right f'      -> Right (e' + f')
safeEval (Mul e f) = case safeEval e of
    Left "error"  -> Left "error"
    Left divE     -> Left divE
    Right e'      -> case safeEval f of
        Left "error"  -> Left "error"
        Left divE     -> Left divE
        Right f'      -> Right (e' * f')
safeEval (Div e f) = case safeEval e of
    Left "error"  -> Left "error"
    Left divE     -> Left divE
    Right e'      -> case safeEval f of
       Left "error"  -> Left "error"
       Left divE     -> Left divE
       Right f'      -> (e' `safeDiv` f')



safeEval' :: Expr -> Either String Int
safeEval' (Con v) = Right v
safeEval' (Add e f) = safeEval' e >>= \e' -> safeEval' f >>= \f' -> return (e' + f')
safeEval' (Mul e f) = safeEval' e >>= \e' -> safeEval' f >>= \f' -> return (e' * f')
safeEval' (Div e f) = safeEval' e >>= \e' -> safeEval' f >>= \f' -> e' `safeDiv` f'



safeEval'' :: Expr -> Either String Int
safeEval'' (Con v) = Right v
safeEval'' (Add e f) = do
                         e' <- safeEval'' e
                         f' <- safeEval'' f
                         return (e' + f')
safeEval'' (Mul e f) = do
                         e' <- safeEval'' e
                         f' <- safeEval'' f
                         return (e' * f')
safeEval'' (Div e f) = do
                         e' <- safeEval'' e
                         f' <- safeEval'' f
                         e' `safeDiv` f'
