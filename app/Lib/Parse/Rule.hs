module Lib.Parse.Rule where
{- `Lib.Parser.Rule` implements `Rule` parser.
    
    TODO: Implement "<-"
 -}

import Text.Megaparsec ( runParser )

import Text.Megaparsec.Char ( string )

import Lib.Parse.Meta ( Parser
                       , reservedChars
                       , singleSpace
                       , skipSpace
                       )

import Lib.Parse.Term ( term )

import Lib.ReductionSystem ( Rule ( Rule )
                           , mkRule
                           , left
                           , right
                           )

import Lib.Term ( mkConstant )

rule :: Parser Rule
{- Returns `Rule` by parsing two seperate `Term`s.

   The parser splits the string along " -> " and calls `term` on both sides.
 -}
rule = do
         a <- term
         b <- singleSpace *> string "->" *> singleSpace *> term
         return Rule {left = a, right = b}

parseRule :: String -> Rule 
{- Returns Rule by interfacing with `rule` parser.

   An abstraction is provided so the user does not have to deal with
   `Megaparsec`.
 -}
parseRule = (.) handler $ runParser rule ""
          where handler (Right x) = x
                handler _ = mkRule (mkConstant "error") (mkConstant "null")


