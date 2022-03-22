module Lib.ReductionSystem where
{- `Lib.Reduction` defines two datatypes `Rule` and `TRS`,
   the primary components of the rewriting system.
 -}
import Data.Foldable ( toList )
import Data.List ( intercalate
                 , elemIndex
                 , length
                 )
import Data.Set.Ordered ( OSet
                        , empty
                        -- , size
                        , elemAt
                        , filter
                        , (|>)
                        , fromList
                        )
import Data.Maybe ( fromJust ) -- only for use when a list is known NOT to be nothing

import Lib.Term ( Term ( Variable
                       , Predicate
                       )
                , symbol
                , parameters
                , varPositions
                , (|=) -- term matching : equals but specificity is set by the right.
                )
import Lib.Utils ( findRepeat )

{- `Rule` contains two `Term`s, the left being the side matched, and the right
   being the result.
 -}
data Rule = Rule { left :: Term, right :: Term } deriving (Eq, Ord)

-- Helper functions for quickly grabbing parameters from a rule.
-- TODO: Pattern match variables to ensure no breakage.
leftside = parameters . left
rightside = parameters . right

instance Show Rule where
{- Convert `Rule` to string that is re-parsable.
 -}
     show (Rule x y) = show x ++ " -> " ++ show y

transform :: Term -> Rule -> Term -- implicit assumption rule and term match
{- Provides the resulting `Term` from the usage of a `Rule

   Warning: `transform` implicitly assumed the term matched (|=)
            with the left side of the rule.

   TODO/WARNING/BUG/HIGH-PRIORITY: This function crashes for certain kinds of
                                   transformations. Fix ASAP!
 -}
transform term rule = if maxIndex /= 0 || not (null rightVars)
                      then Predicate rightName (alter [] 0 rightVars)
                      else right rule
    where 
        maxIndex = (+(-1)) $ length $ rightside rule
        rightVars = varPositions $ right rule
        leftIndex index = elemIndex (rightside rule !! index) (leftside rule)
        pick Nothing index = rightside rule !! index
        pick (Just index) _ =  leftside rule !! index
        alter result i xs 
            | null xs = if maxIndex < i 
                        then result 
                        else case xs of [x] -> result ++ [rightside rule !! i]
                                        _      -> alter (result ++ [rightside rule !! i]) (i+1) (tail xs)
            | i `elem` xs = alter (result ++ [pick (leftIndex i) i]) (i+1) (tail xs)
            | otherwise = alter (result ++ [rightside rule !! i]) (i+1) (tail xs) 
        rightName = symbol $ right rule

{- `TRS` is the (T)erm (R)ewriting (S)ystem.

   `TRS` contains a set of `Rule`s that are order by
   insertion.
 -}
newtype TRS = TRS { rules :: OSet Rule }

newTRS = TRS empty 

generateTRS :: [Rule] -> TRS
generateTRS ruleset = TRS $ fromList ruleset

insertRule :: TRS -> Rule -> TRS
{- Returns a new `TRS` that includes the new rule.
 -}
insertRule trs rule = TRS $ rules trs |> rule

instance Show TRS where
{- Convert `TRS` to string that is re-parsable
 -}
     show trs = intercalate "\n" $ [show x | x <- toList $ rules trs]

{- Attempts to apply a `TRS` to a `Term`.

   If there is no match, the original `Term` is returned.
NOTES:
 this is the 'decision algorithm'. this is what i reallllllly need to improve
 recursion is also done one step at a time, but all args are done simultaneously.
 this will therefore miss a few cases.
 case 1:
   a(b,c) -> d
   e -> b
   b -> x
   g -> f
   f -> c
   therefore a(e,g) -> a(b,f) -> a(x,c)
 case 2:
   q($a) -> $a
 issues come from `transform`
 -}
reduce trs term
    | null possible = case term of
        Variable _ -> term
        Predicate _ [] -> term
        Predicate xSymbol xs -> Predicate xSymbol $ map (trs`reduce`) xs
    | otherwise = transform term $ fromJust $ possible `elemAt` 0
    -- (///) and `match` might do more in the future. 
    -- (///) for example might apply it's own kind of ordering
    where possible = rules trs /// match
          (///) xs f = Data.Set.Ordered.filter f xs
          match = (term |=) . left

normalize :: TRS -> Term -> Term
{- Applies `reduce` until `Term stops changing.

   TODO: Consider using lookaheads to choose which reduction
         path to use.
 -}
normalize trs term = grabResult $ findRepeat reductions
                    where reductions = 100 `take` iterate (trs`reduce`) term -- 100 is used to prevent infinite loop
                          grabResult Nothing = term
                          grabResult (Just x) = x

-- Need to implement functions that check qualities
-- isDeterministic :: TRS -> Bool
-- isDeterministic trs = (==) (size $ rules trs) (size (mapMonotonic left $ rules trs)) -- O(N)
