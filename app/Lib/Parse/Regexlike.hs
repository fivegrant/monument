module Lib.Parse.Regexlike where

{- `Lib.Parser.Regexlike` 
 -}

import Text.Megaparsec ( (<|>) -- composes parser `p` with `q`. (q if p fails to parse)
                       , (<?>) -- overrides parser `p`'s error with string `m`.
                       , between 
                       , noneOf
                       , some
                       , sepBy
                       , notFollowedBy
                       , try
                       , runParser
                       )

import Text.Megaparsec.Char ( char )

import Lib.Parse.Meta ( Parser
                      , singleSpace
                      , reservedChars
                      , skipSpace
                      , name
                      )

import Lib.System.ENFA ( Automaton
                       , mkSingleChar
                       , mkEmpty
                       , (.&)
                       , (.*)
                       , (.|)
                       )

singleChar :: Parser Automaton
singleChar = mkSingleChar <$> noneOf (reservedChars ++ ['+','*'])

group :: Parser Automaton
group = inGroup regexlike
   where inGroup = between (char '(') (char ')')

concatenation :: Parser Automaton
concatenation = foldl (.&) (mkEmpty "") <$> some regexlike

kleine :: Parser Automaton
kleine = (<$>) (.*) $ (<*) regexlike $ char '*'

union :: Parser Automaton
union = uncurry (.|) <$> findTwo
  where findTwo = do
         a <- regexlike 
         b <- char '+' *> regexlike
         return (a,b)


regexlike = try group <|> try concatenation <|> try kleine <|> try union <|> singleChar <?> "regexlike expression"
-- parseRegexlike :: String -> (String -> Bool)
