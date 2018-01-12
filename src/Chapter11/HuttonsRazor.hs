module Chapter11.HuttonsRazor where

data Expr
    = Lit Integer
    | Add Expr Expr

eval :: Expr -> Integer
-- eval (Add x y) = x + y
eval (Lit x) = x
