-- Go through the monadic implementation of `eval :: Expr -> Maybe Value`.
-- Try to implemenet `eval :: Expr -> Either String Value`

type Value = Int

data Expr
  = Lit Value
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving Show


-- with do notation

safeDiv :: Int -> Int -> Either String Value
safeDiv x 0 = Left "Can't Divide By Zero!"
safeDiv x y = Right $ div x y
  
evaL :: Expr -> Either String Value
evaL (Lit v)    = return v
evaL (Add e f)  = do
  v <- evaL e
  w <- evaL f
  Right $ v + w 
evaL (Sub e f)  = do
  v <- evaL e
  w <- evaL f
  Right $ v - w
evaL (Mul e f)  = do
  v <- evaL e
  w <- evaL f
  Right $ v * w
evaL (Div e f)  = do
  v <- evaL e 
  w <- evaL f
  safeDiv v w 
  

-- with Lambdas
  
safeDiv' :: Int -> Int -> Either String Value
safeDiv' x 0 = Left "Can't Divide By Zero!"
safeDiv' x y = Right $ div x y
  
evaL' :: Expr -> Either String Value
evaL' (Lit v)    = return v
evaL' (Add e f)  =
  evaL' e >>= \v ->
  evaL' f >>= \w ->
  Right $ v + w
evaL' (Sub e f) =
  evaL' e >>= \v ->
  evaL' f >>= \w ->
  Right $ v - w
evaL' (Mul e f) =
  evaL' e >>= \v ->
  evaL' f >>= \w ->
  Right $ v * w
evaL' (Div e f) =
  evaL' e >>= \v ->
  evaL' f >>= \w ->
  safeDiv' v w



expr1 = 
  Sub
    (Add
      (Sub (Lit 2) (Lit 3))
      (Mul (Lit 4) (Lit 5)))
    (Div (Lit 6) (Lit 3))

expr2 = 
  Sub
    (Add
      (Sub (Lit 2) (Lit 3))
      (Mul (Lit 4) (Lit 5)))
    (Div (Lit 6) (Lit 0))




