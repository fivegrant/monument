module Lib.System.TRS where
{- `Lib.System.TRS` defines two datatypes `Rule` and `TRS`,
   the primary components of the rewriting system.
 -}
import Data.Foldable ( toList )
import Data.List ( intercalate
                 , elemIndex
                 , length
                 )
import Data.Set.Ordered ( OSet
                        , empty
                        , elemAt
                        , filter
                        , (|>)
                        , fromList
                        )
import Data.Maybe ( fromJust ) -- only for use when a list is known NOT to be nothing

import Lib.Component.Rule ( Rule
                          , valid
                          , transform
                          , left
                          )

import Lib.Component.Term ( Term ( Variable
                                 , Predicate
                                 )
                          , mkFunction
                          , (|=) -- term matching : equals but specificity is set by the right.
                          )

import Lib.Utils.List ( findRepeat )

{- `TRS` is the (T)erm (R)ewriting (S)ystem.

   `TRS` contains a set of `Rule`s that are order by
   insertion.
 -}
newtype TRS = TRS { rules :: OSet Rule }

newTRS = TRS empty 

mkTRS :: [Rule] -> TRS
{- Smart constructor for `TRS`.

   Ensures `TRS` does not accept invalid rules.
 -}
mkTRS ruleset = TRS $ fromList $ Prelude.filter valid ruleset

insertRule :: TRS -> Rule -> TRS
{- Returns a new `TRS` that includes the new rule.

   Ensures `TRS` does not accept invalid rules.

   TODO: Reduce time complexity.
 -}
insertRule trs rule = if valid rule then TRS $ rules trs |> rule else trs

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
        Predicate _ [] _ -> term
        Predicate xSymbol xs _ -> mkFunction xSymbol $ map (trs`reduce`) xs
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
