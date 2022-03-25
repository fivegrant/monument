module Lib.Term where
{- `Lib.Term` defines the basic unit of rewriting, `Term` and its
   functions and constraints.
 -}

import Data.List ( intercalate
                 , findIndices
                 )

{- `Term` represents the base unit of a term rewriting system.

   `Term` can be a variable, function, or constant, however,
   a constant is really just a function with no argument hence
   constant being represented by a `Predicate` with no parameters. 
 -}
data Term = Variable { symbol :: String } |
            Predicate { symbol :: String, parameters :: [Term] } 

isConstant :: Term -> Bool
{- Check if `Term` is constant.
 -}
isConstant (Predicate _ []) = True
isConstant _ = False

containsVar :: Term -> Bool
{- Check if `Term` is parameterized AND contains `Variable`.
 -}
containsVar (Variable _) = True
containsVar (Predicate _ []) = False
containsVar (Predicate _ content) = any containsVar content

varPositions :: Term -> [Int]
{- Provides a list of the `Variable`s indices in a given `Predicate`.
 -}
varPositions (Variable  _) = []
varPositions (Predicate _ []) = []
varPositions (Predicate _ xs) = findIndices isVar xs
    where isVar (Variable _) = True
          isVar _          = False


(|=) :: Term -> Term -> Bool 
{- Match left side with right side. (match operator)

   Functionally, the match operator is similar to equality, however,
   the left side must not contain variables where the rightside contains
   constants. Thus, (|=) is (==) AND not specificity mismatch.

   TODO: Develop a clear definition of `specificity`.
 -}
Variable _ |= Predicate _ _ = False  -- right demands specificity
Variable _ |= Variable _ = True 
Predicate _ _ |= Variable _ = True
Predicate xSymbol [] |= Predicate ySymbol [] = xSymbol == ySymbol
Predicate xSymbol xs |= Predicate ySymbol ys = xSymbol == ySymbol && all match pairs
                                             where match (x,y) = x |= y
                                                   pairs = zip xs ys

instance Show Term where
{- Convert Terms to strings in a form where they can be re-parsed.

   CAVEAT: This actually cannot be parsed back yet because
           lexing whitespace for predicate parameters doesn't work yet.
 -}
        show (Variable x) = "$" ++ x
        show (Predicate constant []) = constant
        show (Predicate name xs) = name ++ "(" ++ contents ++ ")"
            where contents = intercalate ", " [show term | term <- xs]

instance Eq Term where
        Variable x == Variable y = x == y
        Predicate xSymbol [] == Predicate ySymbol [] = xSymbol == ySymbol
        Predicate xSymbol xs == Predicate ySymbol ys = xSymbol == ySymbol && xs == ys
        _ == _ = False

instance Ord Term where -- deprecated, better system needed for organizing terms 
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

