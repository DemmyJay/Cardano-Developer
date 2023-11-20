module InferMonads where

data Expr = Con Int 
          | Add Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving Show

ctd1 :: Expr
ctd1 = Add ( Mul (Div (Con 12) (Con 0)) (Add (Con 2) (Con 1))) (Con 2)
ctd2 = Add ( Mul (Div (Con 12) (Con 4)) (Add (Con 2) (Con 1))) (Con 2)
 
eval :: Expr -> Int
eval (Con v) = v
eval (Add e f) = eval e + eval f
eval (Mul e f) = eval e * eval f
eval (Div e f) = eval e `div` eval f


safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

safeEval :: Expr -> Maybe Int
safeEval (Con v) = Just v
safeEval (Add e f) = case safeEval e of
    Nothing  -> Nothing
    Just e'   -> case safeEval f of
        Nothing -> Nothing
        Just f'  -> Just (e' + f')
safeEval (Mul e f) = case safeEval e of
    Nothing  -> Nothing
    Just e'   -> case safeEval f of
        Nothing -> Nothing
        Just f'  -> Just (e' * f')
safeEval (Div e f) = case safeEval e of
    Nothing  -> Nothing
    Just e'   -> case safeEval f of
        Nothing -> Nothing
        Just f'  -> (e' `safeDiv` f')

-- Apply operator type (aka Shovel it in)

-- (>>=) :: m a -> (a -> m b) -> m b
--          Maybe a -> (a -> Maybe b) -> Maybe b

-- m a >>= k = case m a of 
--               Nothing -> Nothing
--               Just a -> k a

-- return :: a -> m a


safeEval' :: Expr -> Maybe Int
safeEval' (Con v) = Just v
safeEval' (Add e f) = safeEval' e >>= \e' -> safeEval' f >>= \f' -> return (e' + f')
safeEval' (Mul e f) = safeEval' e >>= \e' -> safeEval' f >>= \f' -> return (e' * f')
safeEval' (Div e f) = safeEval' e >>= \e' -> safeEval' f >>= \f' -> e' `safeDiv` f'

safeEval'' :: Expr -> Maybe Int
safeEval'' (Con v) = Just v
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


ioEval :: Expr -> IO Int
ioEval (Con v) = return v
ioEval (Add e f) = putStrLn ("Adding " ++ show e ++ " plus " ++ show f) >>
                   ioEval e >>= \e' -> ioEval f >>= \f' -> return (e' + f')
ioEval (Mul e f) = putStrLn ("Multiplying " ++ show e ++ " and " ++ show f) >>
                   ioEval e >>= \e' -> ioEval f >>= \f' -> return (e' * f')
ioEval (Div e f) = ioEval f >>= \f' ->
                     case f' of
                       0          -> putStrLn "You know you can't divide by zero" >> return 0
                       otherwise  ->
                         putStrLn ("Dividing " ++ show e ++ " by " ++ show f) >>
                         ioEval e >>= \e' -> ioEval f >>= \f' -> return (e' `div` f') 

