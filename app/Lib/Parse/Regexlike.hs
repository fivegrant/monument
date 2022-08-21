module Lib.Parse.Regexlike where

{- `Lib.Parser.Regexlike` 

   The grammar:
   ```
   R -> [R|R] | RR | R* | (R) | \epsilon | t
   ```

 -}

import Text.Megaparsec ( (<|>) -- composes parser `p` with `q`. (q if p fails to parse)
                       , (<?>) -- overrides parser `p`'s error with string `m`.
                       , between 
                       , noneOf
                       , some
                       , try
                       , eof
                       , runParser
                       )

import Text.Megaparsec.Char ( char )

import Lib.Parse.Meta ( Parser
                      , reservedChars
                      , name
                      , (##)
                      )

import Lib.System.ENFA ( Automaton
                       , mkSingleChar
                       , mkNone
                       , mkEmpty
                       , (.&)
                       , (.*)
                       , (.|)
                       , check
                       , tag
                       )

singleChar :: Parser Automaton
singleChar = mkSingleChar <$> noneOf (reservedChars ++ ['|','*','[',']'])

inGroup :: Parser a -> Parser a
inGroup = char '(' `between` char ')'

group :: Parser Automaton
group = inGroup regexlike

namedGroup :: Parser Automaton
namedGroup = tag ## inGroup pair
   where pair = (,)
                   <$> name
                   <*> (char '$' *> regexlike)

word :: Parser Automaton
word = foldl (.&) (mkEmpty "") <$> some singleChar 

concatenation :: Parser Automaton
concatenation =  (.&) ## pair
  where pair = (,)
                  <$> unit
                  <*> (char ' ' *> unit)

kleine :: Parser Automaton
kleine = (<$>) (.*) $ (<*) unit $ char '*'

union :: Parser Automaton
union = (.|) ## inBrac pair
  where pair = (,) 
                  <$> unit 
                  <*> (char '|' *>  unit)
        inBrac = char '[' `between` char ']'

unit :: Parser Automaton
unit = try namedGroup <|> 
       try group <|> 
       try word <|> 
       try union <|>
       singleChar

regexlike :: Parser Automaton
regexlike = try kleine <|> 
            try unit <|>
            try concatenation <?> 
            "regexlike expression"

parseRegexlike :: String -> (String -> Bool)
parseRegexlike = (.) check $ (.) handler $ runParser regexlike ""
          where handler (Right x) = x
                handler _ = mkNone ""

parseAutomaton :: String -> Automaton
parseAutomaton = (.) handler $ runParser regexlike ""
          where handler (Right x) = x
                handler _ = mkNone ""
