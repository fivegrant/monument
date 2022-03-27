module Lib.Term where
{- `Lib.Term` defines the basic unit of rewriting, `Term` and its
   functions and constraints.
 -}

import Data.List ( intercalate
                 , findIndices
                 )
import Data.Maybe ( fromMaybe )
import qualified Data.Map as Map ( Map
                                 , empty
                                 , foldr
                                 , foldrWithKey
                                 , member
                                 , insert
                                 , (!)
                                 , lookup
                                 , fromList
                                 , keys
                                 , null
                                 )


{- `Positions` takes a Function and returns a list of all the `Variable` positions.

   If a `Variable` or constant `Predicate` is given, an empty list is returned.
   The format of the list that is returned contain the indices that need to be 
   visited at each level.

   Example: The predicate `f($x,g($x),$y)` would return the map
            `[0,[1,0]]` for the key `x` and `[2]` for `y`.
 -}
type Positions = Map.Map Term [[Int]]

{- `Term` represents the base unit of a term rewriting system.

   `Term` can be a variable, function, or constant, however,
   a constant is really just a function with no argument hence
   constant being represented by a `Predicate` with no parameters. 
 -}
data Term = Variable { symbol :: String } |
            Predicate { symbol :: String, parameters :: [Term], varLocations :: Positions } 
            deriving (Eq)

genLocations :: [Term] -> Positions
{- Generate a mapping of all the locations of variables.

   Conforms to the conventions set in `Positions` documentation.
 -}
genLocations terms = foldr buildMap (Map.fromList topLevel) indices
  where indices = [0..length terms - 1]
        isVar (Variable _) = True
        isVar _ = False
        topKeys = [terms !! i | i <- indices, isVar $ terms !! i]
        topLevel = [(k, map (:[]) (findIndices (\i -> k == terms !! i) indices)) | k <- topKeys]
        buildMap i positions = Map.foldrWithKey extend positions subpositions
          where term = terms !! i
                subpositions = case term of 
                                    (Variable _) -> Map.empty
                                    _ -> varLocations term
                extend k v currMap
                  | not $ Map.member k currMap = Map.insert k [i:x | x <- v] currMap
                  | otherwise = Map.insert k (currMap Map.! k ++ [i:x | x <- v]) currMap

locate :: Term -> Term -> [[Int]]
{- Query `Term` for locations of a `Variable`.

   Conforms to the conventions set in `Positions` documentation.
 -}
locate (Variable _) query = []
locate (Predicate _ _ locations) query = fromMaybe [] $ Map.lookup query locations

fetch :: Term -> [Int] -> Maybe Term
{- Find `Term` given a list of indices for each layer.

   Conforms to the conventions set in `Positions` documentation.
 -}
fetch (Variable _) indices = Nothing
fetch (Predicate _ params _) [] = Nothing
fetch (Predicate _ params _) [x] = Just $ params !! x
fetch (Predicate _ params _) (x:xs) = params !! x `fetch` xs

mkFunction :: String -> [Term] -> Term
{- Smart constructor for a function.
 -}
mkFunction sym params = Predicate sym params $ genLocations params

mkFunctionWithLocations :: String -> [Term] -> Positions -> Term
{- Smart constructor for a function when varLocations provided.
 -}
mkFunctionWithLocations = Predicate

mkConstant :: String -> Term
{- Smart constructor for a constant.
 -}
mkConstant sym = Predicate sym [] Map.empty

mkVariable :: String -> Term
{- Smart constructor for a variable.
 -}
mkVariable = Variable

isConstant :: Term -> Bool
{- Check if `Term` is constant.
 -}
isConstant (Predicate _ [] _ ) = True
isConstant _ = False

variables :: Term -> [Term]
variables (Variable _) = []
variables (Predicate _ _ positions) = Map.keys positions

containsVar :: Term -> Bool
{- Check if `Term` is parameterized AND contains `Variable`.
 -}
containsVar (Variable _) = True
containsVar (Predicate _ [] _ ) = False
containsVar (Predicate _ _ positions ) = not $ Map.null positions

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

instance Ord Term where  
{- Make `Term` sortable.

   The rules for order have been made pretty arbitrarily. 
 -}
        Variable x <= Variable y = x <= y
        Variable _ <= Predicate {} = False 
        Predicate {} <= Variable _  = True
        Predicate xSymbol [] _ <= Predicate ySymbol [] _ = xSymbol < ySymbol
        Predicate xSymbol xs _ <= Predicate ySymbol ys _ | xSymbol == ySymbol = peek
                                                         | xSymbol <= ySymbol = True
                                                         | otherwise = False
                                                         where pairs = xs `zip` ys
                                                               pairEqual (x,y) = x == y
                                                               remaining = dropWhile pairEqual pairs
                                                               peek | null remaining = True
                                                                    | otherwise = uncurry (<=) $ head remaining

