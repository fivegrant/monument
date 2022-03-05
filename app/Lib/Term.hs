module Lib.Term where

import Data.List (intercalate, findIndices)

data Term = Variable  { symbol :: String } |
            Predicate { symbol :: String, parameters :: [Term] } 

isConstant :: Term -> Bool
isConstant (Predicate _ []) = True
isConstant _ = False

containsVar :: Term -> Bool
containsVar (Variable _) = True
containsVar (Predicate _ []) = False
containsVar (Predicate _ content) = any containsVar content

varPositions :: Term -> [Int]
varPositions (Variable  _) = []
varPositions (Predicate _ []) = []
varPositions (Predicate _ xs) = findIndices isVar xs
    where isVar (Variable _) = True
          isVar _          = False

instance Eq Term where -- all my ideas behind equality could be bad.
        Variable _ == Variable _ = True 
        Variable _ == Predicate _ _ = True 
        Predicate _ [] == Predicate _ [] = True
        Predicate xSymbol xs == Predicate ySymbol ys = xSymbol == ySymbol && xs == ys
        _ == _ = False

instance Show Term where
        show (Variable x) = x
        show (Predicate constant []) = constant
        show (Predicate name xs) = show name ++ "(" ++ contents ++ ")"
            where contents = intercalate ", " [show term |term <- xs]

-- Not sure if `instance Read Term` would be possible
-- to implement given that constants vs variables aren't known
-- ahead of time.


