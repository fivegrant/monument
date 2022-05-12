module Lib.Component.Macro where

{- `Lib.Component.Macro` matches using regex and creates a new term
                         using content from the capture groups.

   WARNING: NOT YET INCLUDED
 -}

import Data.Maybe ( fromJust ) --UNSAFE

import Text.RegexPR ( matchRegexPR )

import Lib.Component.Term ( Term ( Variable
                                 , Predicate
                                 )
                          , symbol
                          , parameters
                          )

import Lib.Utils.List ( compress )

data Macro = Macro { symbolMacro :: String
                   , form        :: String
                   , responses   :: [Term]
                   }


conforms :: Term -> Macro -> Bool
conforms term macro
        | symbol term /= symbolMacro macro = False
        | isVar term = False
        | length (parameters term) /= 1 = False
        | isConstant $ (!!0) $ parameters term = False
        | otherwise = let content = symbol $ (!!0) $  parameters term in 
                      case form macro `matchRegexPR` content of
                          Nothing -> False
                          _ -> True
       where isVar (Variable _) = True
             isVar _ = False
             isConstant (Variable _) = False
             isConstant x = null $ parameters x

abstract :: Term -> Macro -> Term
{- Transforms `Term` using results from the capture group. There
   is an assumption that the capture groups are going to be reversed
   ordered.

   TODO/WARNING: INCOMPLETE
   Warning: `conforms` is expected to have returned `True`.
   TODO: implement `conform` inside of abstract so `matchRegexPR` doesn't have
         to run twice.
 -}
abstract term macro = term
                    where args = compress $ snd $ fromJust $ form macro `matchRegexPR` content 
                          content = symbol $ (!!0) $  parameters term

(>-) = abstract
-- hide :: Term -> Macro -> Term
{- Transforms `Term` into a form that CAN be detected by conforms i.e.
   reversing the results of `abstract`. `abstract . hide . abstract`
   is equivalent to simply `abstract`. `hide . abstract . hide` is equivalent
   to hide.
 -}


-- (-<) = hide
