module Lib.Macro where

{- `Lib.Macro`: The preprocessor/postprocessor which
                converts using user-defined macros.

   WARNING: NOT YET INCLUDED
 -}

{- `Alphabet` lists all the characters to match.

   `Just` enforces characters while `Nothing` is a wildcard.
 -}
type Alphabet = Maybe String

mkAlphabet :: String -> Alphabet
{- `mkAlphabet` Smart constructor for `Alphabet`.
 -}
mkAlphabet [] = Nothing
mkAlphabet x = Just x

{- `Features` contains what should be parsed but discarded.

   `strict` disables parsing multiple whitespace.
 -}
data Features = Features { left      :: Maybe String
                         , separator :: Maybe String
                         , right     :: Maybe String
                         , strict    :: Bool
                         }
                deriving ( Show, Eq )

data Macro = Simple  { alphabet :: Alphabet
                     , features :: Features
                     , name     :: String
                     } |
             Complex { start     :: Macro
                     , end       :: Macro
                     , interrupt :: Maybe String
                     }
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
