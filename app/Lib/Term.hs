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

instance Eq Term where
        Variable _ == Variable _ = True 
        Variable _ == Predicate _ _ = True 
        Predicate xSymbol [] == Predicate ySymbol [] = xSymbol == ySymbol
        Predicate xSymbol xs == Predicate ySymbol ys = xSymbol == ySymbol && xs == ys
        _ == _ = False

instance Ord Term where -- Ord will probably be deprecated in place of line number
        Variable _ <= Variable _ = True 
        Variable _ <= Predicate _ _ = True 
        Predicate _ _ <= Variable _  = False
        Predicate xSymbol [] <= Predicate ySymbol [] = xSymbol < ySymbol
        Predicate xSymbol xs <= Predicate ySymbol ys | xSymbol == ySymbol = peek
                                                     | xSymbol <= ySymbol = True
                                                     | otherwise = False
                                                     where pairs = xs `zip` ys
                                                           pairEqual (x,y) = x == y
                                                           remaining = dropWhile pairEqual pairs
                                                           peek | null remaining = True
                                                                | otherwise = uncurry (<=) $ head remaining

instance Show Term where
        show (Variable x) = "$" ++ x
        show (Predicate constant []) = constant
        show (Predicate name xs) = name ++ "(" ++ contents ++ ")"
            where contents = intercalate ", " [show term | term <- xs]

-- Not sure if `instance Read Term` would be possible
-- to implement given that constants vs variables aren't known
-- ahead of time.


