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
                , containsVar
                , (|=) -- term matching : equals but specificity is set by the right.
                )
import Lib.Utils ( findRepeat )

{- `Rule` contains two `Term`s, the left being the side matched, and the right
   being the result.
 -}
data Rule = Rule { left :: Term, right :: Term } deriving (Eq, Ord)

valid :: Rule -> Bool
{- Checks if rules follow certain properties

   In order for the system to work, the following cases need to be removed:
    1. The left rule cannot be a single variable
    2. Every variable on the right must be found on the left.
 -}
valid (Rule (Variable _) r) = False
valid (Rule l r) = any onLeft $ parameters r
           where onLeft = (`elem` parameters l)

-- TODO: HANDLE ERRORS WITH OWN TYPE + FILE
invalid = Predicate "error:invalid_rule" [] -- error returned when invalid

instance Show Rule where
{- Convert `Rule` to string that is re-parsable.
 -}
     show (Rule x y) = show x ++ " -> " ++ show y

transform :: Term -> Rule -> Term -- implicit assumption rule and term match
{- Provides the resulting `Term` from the usage of a `Rule

   Warning: `transform` implicitly assumed the term matched (|=)
            with the left side of the rule.

   TODO/WARNING/BUG/HIGH-PRIORITY: `f(f($var)) -> $var` not working
   TODO: Error checking with `valid`?
   TODO: Shorten length of lines
 -}
transform (Predicate _ termArgs)
          (Rule (Predicate _ ruleArgs) 
                (Variable x)) = locate index
  where index = elemIndex (Variable x) ruleArgs
        locate Nothing = invalid
        locate (Just i) = termArgs !! i -- unsafe (!!), 
                                        -- honestly I'm tempted to rewrite this case with `fromJust`

transform (Predicate termSym termArgs)
          (Rule (Predicate _ lArgs) 
                (Predicate predSym rArgs)) = if not $ containsVar $ Predicate predSym rArgs
                                             then Predicate predSym rArgs
                                             else Predicate predSym $ map pick rArgs
                                               where leftIndex x = fromJust $ elemIndex x lArgs
                                                     pick x = case x of 
                                                               Predicate _ _ -> x
                                                               Variable _ -> (!!) termArgs $ leftIndex x

transform x y = Predicate "error:no_match" []

{- `TRS` is the (T)erm (R)ewriting (S)ystem.

   `TRS` contains a set of `Rule`s that are order by
   insertion.

   TODO: Run `valid` to see if a rule can be added.
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
