module Lib.Parse.Parser (parseTerm, parseRule, isComment) where

{- `Lib.Parse.Parser` exposes all the necessary parsers.
 -}

import Lib.Parse.Term ( parseTerm )
import Lib.Parse.Rule ( parseRule )
import Lib.Parse.Comment ( isComment )
