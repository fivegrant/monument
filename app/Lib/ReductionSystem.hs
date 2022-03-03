module Lib.ReductionSystem where

import Data.Sequence

import Lib.Term

data Rule = Rule { left :: Term, right :: Term } deriving (Eq)

match :: Term -> Rule -> Bool
match term rule = term == left rule

transform :: Term -> Rule -> Term
transform term rule = term -- TODO: Implement

type TRS = Seq Rule
-- `instance Show TRS` should export to the format that
-- `instance Read TRS` will use. This would allow
-- for dynamic editing in the interpreter which could
-- save new rules to the file.
