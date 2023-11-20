module Maybe where

{-

-- eval :: Expr -> Maybe Value
-- eval (Lit v)   = Just v
-- eval (Add e f) = case eval e of
--    Nothing -> Nothing
--    Just v  -> case eval f of
--    Nothing -> Nothing
--    Just w  -> Just $ v + w
-- eval (Sub e f) = case eval e of
--    Nothing -> Nothing
--    Just v  -> case eval f of
--    Nothing -> Nothing
--    Just w  -> Just $ v - w
-- eval (Mul e f) = case eval e of
--    Nothing -> Nothing
--    Just v  -> case eval f of
--    Nothing -> Nothing
--    Just w  -> Just $ v * w
-- eval (Div e f)  = case eval e of
--    Nothing -> Nothing
--    Just v  -> case eval f of
--    Nothing -> Nothing
--    Just w  -> safeDiv v w

-}


{-

  ------------------ Implementing the Maybe Monad -------------------

return :: a -> Maybe a
return = Just

-- (>>=)  :: Maybe a -> (a -> Maybe b) -> Maybe b
-- ma >>= f = case ma of
--    Nothing -> Nothing
--    Just a  -> k a

-}


type Value = Int
data Expr
  = Lit Value
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr

safeDiv :: Int -> Int -> Maybe Int
safeDiv x 0 = Nothing
safeDiv x y = Just $ div x y

eval :: Expr -> Maybe Value
eval (Lit v)    = return v
eval (Add e f)  = do
  v <- eval e     -- eval e >>= \v ->
  w <- eval f     -- eval f >>= \w ->
  return $ v + w  -- return $ v + w
eval (Sub e f)  = do
  v <- eval e     -- eval e >>= \v ->
  w <- eval f     -- eval f >>= \w ->
  return $ v - w  -- return $ v - w
eval (Mul e f)  = do
  v <- eval e     -- eval e >>= \v ->
  w <- eval f     -- eval f >>= \w ->
  return $ v * w  -- return $ v * w
eval (Div e f)  = do
  v <- eval e     -- eval e >>= \v ->
  w <- eval f     -- eval f >>= \w ->
  safeDiv v w     -- safeDiv v w

e = 
  Sub
    (Add
      (Sub (Lit 2) (Lit 3))
      (Mul (Lit 4) (Lit 5)))
    (Div (Lit 6) (Lit 3))

one :: Int -> Maybe Int
one x = if x < 7 then return (x + 2) else Nothing

two :: Int -> Maybe Int
two y = if y < 12 then return (y * 3) else Nothing

three :: Int -> Maybe Int
three z = if z < 4 then return (z ^ 2) else Nothing

h :: Int -> Int -> Int -> Maybe Int
h x y z =
  one x   >>= \u ->
  two y   >>= \v ->
  three z >>= \w ->
  return $ u + v + w

-- Go through the monadic implementation of `eval :: Expr -> Maybe Value`.
-- Try to implement `eval :: Expr -> Either String Value`

data Error
  = DivisionByZeroError
  | FileNotFoundError
  | NetworkError
  | NegativeSqrtError

{-
safeDiv' :: Int -> Int -> Either Error Value
safeDiv' x 0 = Left "Error: Division By Zero!"
safeDiv' x y = Right $ div x y
-}




