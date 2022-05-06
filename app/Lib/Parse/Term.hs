module Lib.Parse.Term where

{- `Lib.Parser` implements a `Term` parser. Conveniently, the AST *is*
    already the type that we work with, so no intermediary representation is used.
 -}

import Text.Megaparsec ( (<|>) -- composes parser `p` with `q`. (q if p fails to parse)
                       , (<?>) -- overrides parser `p`'s error with string `m`.
                       , between 
                       , sepBy
                       , notFollowedBy
                       , try
                       , runParser
                       )

import Text.Megaparsec.Char ( char )

import Lib.Parse.Meta ( Parser
                      , singleSpace
                      , skipSpace
                      , name
                      )

import Lib.Term ( Term ( Predicate
                       , Variable
                       )
                , mkFunction
                , mkConstant
                , mkVariable
                , symbol
                , parameters
                )

variable :: Parser Term
{- Returns a `Variable` value.

   The parser simply reads a string of the form $variableName
   and tosses the `$` symbol.
 -}

variable = mkVariable <$> (char '$' *> name)

function :: Parser Term
{- Returns a `Predicate` value that contains parameters.

   The parser matches predicates that contain arguments. `term` is
   called recursively for each of the parameters.
 -}
function = try traditional <|> binary


     where traditional = do 
              functionSymbol <- name
              contents <- inParen params
              return $ mkFunction functionSymbol contents
           binary = inParen operator
           inParen = between (char '(' *> skipSpace) (skipSpace <* char ')')
           params = term `sepBy` (skipSpace *> char ',' *> skipSpace)
           operator = do
              left <- term
              functionSymbol <- singleSpace *> name
              right <- singleSpace *> term
              return $ mkFunction functionSymbol [left,right]

constant :: Parser Term
{- Returns a `Predicate` value that contains no parameters.

   The parser matches contansts which are predicates with no parameters.
   For ease of use, users do not have to define a predicate with `()`,
   which the parser recognizes as a constant. Special rules for parsing
   constants might come along later, so this parser is separate.
 -}
constant = mkConstant <$> (name <* notFollowedBy (skipSpace *> char '('))

term :: Parser Term
{- Returns `Term`.

   The parser attempts to match a constant. If the `constant` parser fails,
   `function` is called. If `function` fails, `variable` is called.
-}
term = try constant <|> try function <|> variable <?> "a well-formed term"

parseTerm :: String -> Term 
{- Returns `Term` by interfacing with `term` parser.

   Again, an abstraction is provided so the user does not have to deal with
   `Megaparsec`.
 -}
parseTerm = (.) handler $ runParser term ""
          where handler (Right x) = x
                handler _ = mkConstant "unreadable"
