module Lib.Component.Rule where
{- `Lib.Component.Rule` defines two datatypes `Rule` and `TRS`,
   the primary components of the rewriting system.
 -}
import qualified Data.Map as Map ( keys
                                 , intersection
                                 , (!)
                                 )
import Data.Maybe ( fromJust ) -- only for use when a list is known NOT to be nothing

import Lib.Component.Term ( Term ( Variable
                                 , Predicate
                                 )
                          , mkFunction
                          , mkConstant
                          , mkVariable
                          , varLocations
                          , variables
                          , locate
                          , fetch
                          , swap
                          )

import Lib.Utils.List ( findRepeat )

{- `Rule` contains two `Term`s, the left being the side matched, and the right
   being the result.
 -}
data Rule = Rule { left  :: Term,
                   right :: Term 
                 } 
            deriving ( Eq, Ord )

mkRule :: Term -> Term -> Rule
{- Smart constructor for `Rule`.
 -}
mkRule = Rule

valid :: Rule -> Bool
{- Checks if rules follow certain properties

   In order for the system to work, the following cases need to be removed:
    1. The left rule cannot be a single variable
    2. Every variable on the right must be found on the left.
 -}
valid (Rule (Variable _) r) = False
valid (Rule l r) = all onLeft $ variables r
           where onLeft = (`elem` variables l)

instance Show Rule where
{- Convert `Rule` to string that is re-parsable.
 -}
     show (Rule x y) = show x ++ " -> " ++ show y

transform :: Term -> Rule -> Term -- implicit assumption rule and term match
{- Provides the resulting `Term` from the usage of a `Rule

   Warning: `transform` implicitly assumed the term matched (|=)
            with the left side of the rule.

   TODO: Error checking with `valid`?
 -}
transform (Variable _) _ = mkConstant "error:variable_query"

transform _ (Rule (Variable _) _) = mkConstant "error:invalid_rule"

transform query (Rule l (Variable x)) = fromJust $ fetch query $ (!!0) $ locate l $ mkVariable x

transform query (Rule l r) = if null $ variables l
                             then r
                             else foldr convert r locations
                               where correspond = fromJust . fetch query . head . locate l
                                     locations = [(k, correspond k) | k <- Map.keys $ Map.intersection (varLocations l) (varLocations r)]
                                     convert (k, new) term = swap term new $ varLocations r Map.! k

