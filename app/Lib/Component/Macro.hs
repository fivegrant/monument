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
{- Transforms `Term` using results from the capture group.

   TODO/WARNING: INCOMPLETE
   Warning: `conforms` is expected to have returned `True`.
   TODO: implement `conform` inside of abstract so `matchRegexPR` doesn't have
         to run twice.
 -}

abstract term macro = term
                    where args = reverse $ snd $ fromJust $ form macro `matchRegexPR` content 
                          content = symbol $ (!!0) $  parameters term

(>-) = abstract
-- hide :: Term -> Macro -> Term
{- Transforms `Term` into a form that CAN be detected by conforms i.e.
   reversing the results of `abstract`. `abstract . hide . abstract`
   is equivalent to simply `abstract`. `hide . abstract . hide` is equivalent
   to hide.
-}


-- (-<) = hide

--- PAST ATTEMPT AT IMPLEMENTATION BELOW; WILL BE SCRAPPED SOON

{- `Alphabet` lists all the characters to match.

   `Just` enforces characters while `Nothing` is a wildcard.
 -}
{---
type Alphabet = Maybe String

---}
{---
mkAlphabet :: String -> Alphabet
---}
{- `mkAlphabet` Smart constructor for `Alphabet`.
 -}
{---
mkAlphabet [] = Nothing
mkAlphabet x = Just x
---}

{- `Features` contains what should be parsed but discarded.

   `strict` disables parsing multiple whitespace.
 -}
{---
data Features = Features { left      :: Maybe String
                         , separator :: Maybe String
                         , right     :: Maybe String
                         , strict    :: Bool
                         }
                deriving ( Show, Eq )
---}

{---
data Macro = Simple  { alphabet :: Alphabet
                     , features :: Features
                     , name     :: String
                     } |
             Complex { start     :: Macro
                     , end       :: Macro
                     , interrupt :: Maybe String
                     }
---}
{- -}
-- int   = Macro (Just ['1'..'9']) (Features Nothing    Nothing    Nothing    True)  (parseTerm "int($target, $previous"))
-- list  = Macro (Nothing)         (Features (Just "[") (Just " ") (Just "]") False) (parseTerm "cons($target, $previous)")
-- nine  = Macro (Just ['9'])      (Features Nothing    Nothing    Nothing    True)  (parseTerm "digit(8)")
-- eight = Macro (Just ['8'])      (Features Nothing    Nothing    Nothing    True)  (parseTerm "digit(7)") 
-- seven = Macro (Just ['7'])      (Features Nothing    Nothing    Nothing    True)  (parseTerm "digit(6)")
-- six   = Macro (Just ['6'])      (Features Nothing    Nothing    Nothing    True)  (parseTerm "digit(5)")
-- five  = Macro (Just ['4'])      (Features Nothing    Nothing    Nothing    True)  (parseTerm "digit(4)")
-- four  = Macro (Just ['4'])      (Features Nothing    Nothing    Nothing    True)  (parseTerm "digit(3)")
-- three = Macro (Just ['3'])      (Features Nothing    Nothing    Nothing    True)  (parseTerm "digit(2)")
-- two   = Macro (Just ['2'])      (Features Nothing    Nothing    Nothing    True)  (parseTerm "digit(1)")
-- one   = Macro (Just ['1'])      (Features Nothing    Nothing    Nothing    True)  (parseTerm "digit(0)")

-- dec 

-- mkMacro
-- match
-- replace

-- MRS -- the TRS analog
-- abstract :: MRS -> String -> String
-- hide :: MRS -> String -> String
{- -}
