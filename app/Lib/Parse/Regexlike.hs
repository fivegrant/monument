module Lib.Parse.Regexlike where

{- `Lib.Parser.Regexlike` 
 -}

import Text.Megaparsec ( (<|>) -- composes parser `p` with `q`. (q if p fails to parse)
                       , (<?>) -- overrides parser `p`'s error with string `m`.
                       , between 
                       , noneOf
                       , some
                       , try
                       , runParser
                       )

import Text.Megaparsec.Char ( char )

import Lib.Parse.Meta ( Parser
                      , reservedChars
                      )

import Lib.System.ENFA ( Automaton
                       , mkSingleChar
                       , mkNone
                       , mkEmpty
                       , (.&)
                       , (.*)
                       , (.|)
                       , check
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

parseRegexlike :: String -> (String -> Bool)
parseRegexlike = (.) check $ (.) handler $ runParser regexlike ""
          where handler (Right x) = x
                handler _ = mkNone ""
