module Lib.Parser where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

import Lib.Term
import Lib.ReductionSystem
import Lib.Utils (findRepeat)

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
