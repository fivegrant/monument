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
                      , (##)
                      )

import Lib.Parse.Term ( term )

import Lib.Component.Rule ( Rule ( Rule )
                          , mkRule
                          , left
                          , right
                          )

import Lib.Component.Term ( mkConstant )

rule :: Parser Rule
{- Returns `Rule` by parsing two seperate `Term`s.

   The parser splits the string along " -> " and calls `term` on both sides.
 -}
rule = mkRule ## pair
  where pair = (,)
               <$> term
               <*> (singleSpace *> string "->" *> singleSpace *> term)

parseRule :: String -> Rule 
{- Returns Rule by interfacing with `rule` parser.

   An abstraction is provided so the user does not have to deal with
   `Megaparsec`.
 -}
parseRule = (.) handler $ runParser rule ""
          where handler (Right x) = x
                handler _ = mkRule (mkConstant "error") (mkConstant "null")


