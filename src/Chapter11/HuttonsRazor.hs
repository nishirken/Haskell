module Chapter11.HuttonsRazor (Expr (Lit, Add), eval, printExpr) where

data Expr
    = Lit Integer
    | Add Expr Expr deriving Show

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y

printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add x y) = printExpr x ++ " + " ++ printExpr y
