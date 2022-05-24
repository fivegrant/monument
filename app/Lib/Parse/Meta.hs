module Lib.Parse.Meta where
{-  `Lib.Parser.Meta`: The shared components and standards of each component's
     parser.

    TODO: - implement lexer allow flexibility with whitespaces.
          - parse full `TRS` instead of passing a list of lines.
          - handle instead of ignore errors

 -}

import Data.Void ( Void )
import Text.Megaparsec ( Parsec
                       , some
                       , many
                       , noneOf
                       )

import Text.Megaparsec.Char ( char )

type Parser = Parsec Void String

-- characters that cannot be used to define a predicate or function name
reservedChars = [' ', '(', ')', ',', '\\', '$']
reservedNames = ["->", "<-", ">-", "-<"] -- UNUSED
singleSpace = some $ char ' ' :: Parser String
skipSpace = many $ char ' '   :: Parser String
consPair = (<$>) . uncurry :: (a -> b -> c) -> Parser (a,b) -> Parser c
(##) = consPair
infixr 2 ##

name :: Parser String
{- Parses string that does not contain reserved characters

   The different components of the term parser end up using this because
   no difference is enforced between variables and predicates.
 -}
name = some $ noneOf reservedChars

