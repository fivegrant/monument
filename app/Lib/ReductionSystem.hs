module Lib.ReductionSystem where

import Data.List (intercalate, elemIndex)
import Data.Set (Set, empty, size, mapMonotonic, elemAt, filter, toList, insert, fromList)

import Lib.Term
import Lib.Utils (findRepeat)

data Rule = Rule { left :: Term, right :: Term } deriving (Eq, Ord)

leftside = parameters . left
rightside = parameters . right

instance Show Rule where
     show (Rule x y) = show x ++ " -> " ++ show y

match :: Term -> Rule -> Bool
match term rule = term == left rule

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


newtype TRS = TRS {rules :: Set Rule}

newTRS = TRS empty 

generateTRS :: [Rule] -> TRS
generateTRS ruleset = TRS $ fromList ruleset

insertRule :: TRS -> Rule -> TRS
insertRule trs rule = TRS $ rule `insert` rules trs

instance Show TRS where
     show trs = intercalate "\n" $ [show x | x <- toList $ rules trs]

-- reduce does not handle subexpressions
-- right now it's just matching the first rule
-- should matter to
reduce :: TRS -> Term -> Term 
reduce trs term
                | null possible = term
                | otherwise = transform term $ elemAt 0 possible
                where possible = Data.Set.filter (term`match`) $ rules trs

normalize :: TRS -> (Term -> Term)
normalize trs term = grabResult $ findRepeat reductions
                    where reductions = 100 `take` iterate (trs`reduce`) term -- 100 is used to prevent infinite loop
                          grabResult Nothing = term
                          grabResult (Just x) = x

isDeterministic :: TRS -> Bool
isDeterministic trs = (==) (size $ rules trs) (size (mapMonotonic left $ rules trs)) -- O(N)
-- `instance Show TRS` should export to the format that
-- `instance Read TRS` will use. This would allow
-- for dynamic editing in the interpreter which could
-- save new rules to the file.
--
--
-- Need to implement functions that check qualities
