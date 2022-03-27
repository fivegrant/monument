module Lib.Parser where

{- `Lib.Parser` implements two parsers: parseTerm and parseRule which
    return their respective types `Term` and `Rule`. Conveniently, the AST *is*
    already the type that we work with, so no intermediary representation is used.
    The parser is built using a parser combinator library, allowing us to build
    a parser by composing smaller ones.

    TODO: - implement lexer allow flexibility with whitespaces.
          - parse full `TRS` instead of passing a list of lines.
 -}

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
                , mkFunction
                , mkConstant
                , mkVariable
                , symbol
                , parameters
                )
import Lib.ReductionSystem ( Rule ( Rule )
                           , mkRule
                           , left
                           , right
                           )

type Parser = Parsec Void String

-- characters that cannot be used to define a predicate or function name
reservedChars = [' ', '(', ')', ',', '\\', '$']

name :: Parser String
{- Parses string that does not contain reserved characters

   The different components of the term parser end up using this because
   no difference is enforced between variables and predicates.
 -}
name = some $ noneOf reservedChars

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
function = do 
              functionSymbol <- name
              contents <- inParen params
              return $ mkFunction functionSymbol contents
      where inParen = between (char '(') (char ')')
            params = term `sepBy` char ','

constant :: Parser Term
{- Returns a `Predicate` value that contains no parameters.

   The parser matches contansts which are predicates with no parameters.
   For ease of use, users do not have to define a predicate with `()`,
   which the parser recognizes as a constant. Special rules for parsing
   constants might come along later, so this parser is separate.
 -}
constant = mkConstant <$> (name <* notFollowedBy (char '('))

term :: Parser Term
{- Returns `Term`.

   The parser attempts to match a constant. If the `constant` parser fails,
   `function` is called. If `function` fails, `variable` is called.
-}
term = try constant <|> try function <|> variable <?> "a well-formed term"

rule :: Parser Rule
{- Returns `Rule` by parsing two seperate `Term`s.

   The parser splits the string along " -> " and calls `term` on both sides.
 -}
rule = do
         a <- term
         b <- string " -> " *> term
         return Rule {left = a, right = b}

parseRule :: String -> Rule 
{- Returns Rule by interfacing with `rule` parser.

   An abstraction is provided so the user does not have to deal with
   `Megaparsec`.
 -}
parseRule = (.) handler $ runParser rule ""
          where handler (Right x) = x
                handler _ = mkRule (mkConstant "error") (mkConstant "null")

parseTerm :: String -> Term 
{- Returns `Term` by interfacing with `term` parser.

   Again, an abstraction is provided so the user does not have to deal with
   `Megaparsec`.
 -}
parseTerm = (.) handler $ runParser term ""
          where handler (Right x) = x
                handler _ = mkConstant "unreadable"

