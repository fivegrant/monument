module Lib.Term where

data Term = Constant String |
            Variable String |
            Expr     [Term]

instance Eq Term where
        Constant x == Constant y = x == y
        Variable _ == Variable _ = True
        Expr [] == Expr [] = True
        Expr (x:xs) == Expr (y:ys) = x == y && Expr xs == Expr ys
        _ == _ = False

instance Show Term where
        show (Constant x) = x
        show (Variable x) = x
        show (Expr []) = ""
        show (Expr (x:xs)) = show x ++ show (Expr xs)

-- Not sure if `instance Read Term` would be possible
-- to implement given that constants vs variables aren't known
-- ahead of time.


