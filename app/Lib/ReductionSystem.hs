module Lib.ReductionSystem where

import Data.Foldable (toList)
import Data.List (intercalate, elemIndex)
import Data.Set.Ordered (OSet, empty, size, elemAt, filter, (|>), fromList)
import Data.Maybe (fromJust) -- only for use when a list is known NOT to be nothing

import Lib.Term
import Lib.Utils (findRepeat)

data Rule = Rule { left :: Term, right :: Term } deriving (Eq, Ord)

leftside = parameters . left
rightside = parameters . right

instance Show Rule where
     show (Rule x y) = show x ++ " -> " ++ show y

match :: Term -> Rule -> Bool
match term rule = term |= left rule 

transform :: Term -> Rule -> Term -- implicit assumption that you already ran `match`
transform term rule = Predicate rightName (alter [] 0 rightVars)
    where rightVars = varPositions $ right rule
          leftIndex index = elemIndex (rightside rule !! index) (leftside rule)
          pick Nothing index = rightside rule !! index
          pick (Just index) _ =  leftside rule !! index
          alter result i xs 
                            | null xs = result
                            | i `elem` xs = alter (pick (leftIndex i) i : result) (i+1) (tail xs)
                            | otherwise = alter ((rightside rule !! i) : result) (i+1) (tail xs)
          rightName = symbol $ right rule


newtype TRS = TRS { rules :: OSet Rule }

newTRS = TRS empty 

generateTRS :: [Rule] -> TRS
generateTRS ruleset = TRS $ fromList ruleset

insertRule :: TRS -> Rule -> TRS
insertRule trs rule = TRS $ rules trs |> rule

instance Show TRS where
     show trs = intercalate "\n" $ [show x | x <- toList $ rules trs]

{-
 this is the 'decision algorith'. this is what i reallllllly need to improve
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
reduce :: TRS -> Term -> Term 
reduce trs term
                | null possible = case term of
		                       Variable _ -> term
				       Predicate _ [] -> term
				       Predicate xSymbol xs -> Predicate xSymbol $ map (trs`reduce`) xs
                | otherwise = transform term $ fromJust $ possible `elemAt` 0
                where possible = Data.Set.Ordered.filter (term`match`) $ rules trs

normalize :: TRS -> Term -> Term
normalize trs term = grabResult $ findRepeat reductions
                    where reductions = 100 `take` iterate (trs`reduce`) term -- 100 is used to prevent infinite loop
                          grabResult Nothing = term
                          grabResult (Just x) = x

-- Need to implement functions that check qualities
-- isDeterministic :: TRS -> Bool
-- isDeterministic trs = (==) (size $ rules trs) (size (mapMonotonic left $ rules trs)) -- O(N)
