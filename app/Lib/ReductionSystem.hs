module Lib.ReductionSystem where

import Data.List (elemIndex)
import Data.Set (Set, size, mapMonotonic)

import Lib.Term

data Rule = Rule { left :: Term, right :: Term } deriving (Eq)

leftside = parameters . left
rightside = parameters . right

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
                            | i `elem` xs = alter ((pick (leftIndex i) i) : result) (i+1) (tail xs)
                            | otherwise = alter (((rightside rule) !! i) : result) (i+1) (tail xs)
          rightName = symbol $ right rule


type TRS = Set Rule

isDeterministic :: TRS -> Bool
isDeterministic trs = (==) (size trs) (size (mapMonotonic left trs)) -- O(N)
-- `instance Show TRS` should export to the format that
-- `instance Read TRS` will use. This would allow
-- for dynamic editing in the interpreter which could
-- save new rules to the file.
--
--
-- Need to implement functions that check qualities
