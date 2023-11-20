type Value = Int
data Expr
  = Lit Value
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr

eval :: Expr -> Value
eval (Lit v)    = v
eval (Add e f)  = eval e + eval f
eval (Sub e f)  = eval e - eval f
eval (Mul e f)  = eval e * eval f
eval (Div e f)  = div (eval e) (eval f)

expr =
  Sub
    (Add (Sub (Lit 2) (Lit 3)) (Mul (Lit 4) (Lit 5)))
    (Div (Lit 6) (Lit 3))
