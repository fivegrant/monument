module Lib.Parser where

import Data.Void ( Void )
import Text.Megaparsec ( Parsec 
                       , (<|>) -- composes parser `p` with `q`. (q if p fails to parse)
                       , (<?>) -- overrides parser `p`'s error with string `m`.
                       , between 
                       , some
                       , noneOf
                       , sepBy
                       , notFollowedBy
                       , try
                       , runParser
                       )
import Text.Megaparsec.Char ( char
                            , string
                            )

import Lib.Term ( Term ( Predicate
                       , Variable
                       )
                , symbol
                , parameters
                )
import Lib.ReductionSystem ( Rule ( Rule )
                           , left
                           , right
                           )

type Parser = Parsec Void String

reservedChars = [' ', '(', ')', ',', '\\', '$']

name :: Parser String
name = some $ noneOf reservedChars

variable :: Parser Term
variable = Variable <$> (char '$' *> name)

function :: Parser Term
function = do 
              functionSymbol <- name
              contents <- inParen params
              return Predicate { symbol = functionSymbol, parameters = contents }
      where inParen = between (char '(') (char ')')
            params = term `sepBy` char ','

constant :: Parser Term
constant = (`Predicate`[]) <$> (name <* notFollowedBy (char '('))

term :: Parser Term
term = try constant <|> try function <|> variable <?> "a well-formed term"

rule :: Parser Rule
rule = do
         a <- term
         b <- string " -> " *> term
         return Rule {left = a, right = b}

parseRule :: String -> Rule 
parseRule = (.) handler $ runParser rule ""
          where handler (Right x) = x
                handler _ = Rule { left = Variable "error", 
                                   right = Variable "null" }

parseTerm :: String -> Term 
parseTerm = (.) handler $ runParser term ""
          where handler (Right x) = x
                handler _ = Predicate "unreadable" []

-- need to work on my error handling
-- lexer needed to reduce whitespace sensitivity
