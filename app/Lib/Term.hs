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
            Predicate { symbol :: String, parameters :: [Term], varLocations :: [[Int]] } 
            deriving (Eq)

mkFunction :: String -> [Term] -> Term
{- Smart constructor for a function.
 -}
mkFunction sym params = Predicate sym params []

mkConstant :: String -> Term
{- Smart constructor for a constant.
 -}
mkConstant sym = Predicate sym [] []

mkVariable :: String -> Term
{- Smart constructor for a variable.
 -}
mkVariable = Variable

isConstant :: Term -> Bool
{- Check if `Term` is constant.
 -}
isConstant (Predicate _ [] _ ) = True
isConstant _ = False

containsVar :: Term -> Bool
{- Check if `Term` is parameterized AND contains `Variable`.
 -}
containsVar (Variable _) = True
containsVar (Predicate _ [] _ ) = False
containsVar (Predicate _ content _ ) = any containsVar content

varPositions :: Term -> [Int]
{- Provides a list of the `Variable`s indices in a given `Predicate`.
 -}
varPositions (Variable  _) = []
varPositions (Predicate _ [] _) = []
varPositions (Predicate _ xs _) = findIndices isVar xs
    where isVar (Variable _) = True
          isVar _          = False

(|=) :: Term -> Term -> Bool 
{- Match left side with right side. (match operator)

   Functionally, the match operator is similar to equality, however,
   the left side must not contain variables where the rightside contains
   constants. Thus, (|=) is (==) AND not specificity mismatch.

   TODO: Develop a clear definition of `specificity`.
 -}
Variable _ |= Predicate {} = False  -- right demands specificity
Variable _ |= Variable _ = True 
Predicate {} |= Variable _ = True
Predicate xSymbol [] _ |= Predicate ySymbol [] _ = xSymbol == ySymbol
Predicate xSymbol xs _ |= Predicate ySymbol ys _ = xSymbol == ySymbol && all match pairs
                                             where match (x,y) = x |= y
                                                   pairs = zip xs ys

instance Show Term where
{- Convert Terms to strings in a form where they can be re-parsed.

   CAVEAT: This actually cannot be parsed back yet because
           lexing whitespace for predicate parameters doesn't work yet.
 -}
        show (Variable x) = "$" ++ x
        show (Predicate constant [] _) = constant
        show (Predicate name xs _) = name ++ "(" ++ contents ++ ")"
            where contents = intercalate ", " [show term | term <- xs]

instance Ord Term where -- deprecated, better system needed for organizing terms 
        Variable _ <= Variable _ = True 
        Variable _ <= Predicate {} = True 
        Predicate {} <= Variable _  = False
        Predicate xSymbol [] _ <= Predicate ySymbol [] _ = xSymbol < ySymbol
        Predicate xSymbol xs _ <= Predicate ySymbol ys _ | xSymbol == ySymbol = peek
                                                         | xSymbol <= ySymbol = True
                                                         | otherwise = False
                                                         where pairs = xs `zip` ys
                                                               pairEqual (x,y) = x == y
                                                               remaining = dropWhile pairEqual pairs
                                                               peek | null remaining = True
                                                                    | otherwise = uncurry (<=) $ head remaining

