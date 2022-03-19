module Lib.Reader where

import Text.Parsec
import Text.Parsec.Char (noneOf, char)
import Text.Parsec.Token as Token

import Lib.Term
import Lib.ReductionSystem
import Lib.Utils (findRepeat)

reservedChars = [' ', '(', ')', ',', '\\']

name = many $ noneOf reservedChars

variable :: Parsec String st Term
variable = char '\\' *> name

predicate = inParen params
       where inParen = between (char '(') (char ')')
             params = sepBy1 $ char ','
-- manyTill validChar $ try params

-- parseTerm :: String -> Either ParseError Term
--- decide if constant, function, variable use combinator 
----combine using ((varParser <|> functionParser) <|> constant parser) <?> "Invalid"
--- *parse zero input functions separately

parseTRS :: String -> TRS
parseTRS _ = sepBy1 $ char '\n'
-- parse each line individually
--- split line at " -> "
--- run parseTerm

