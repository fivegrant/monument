module Lib.Parse.Comment where
{- `Lib.Parse.Comment` detects a comment

   TODO: Parse comments instead of checking
 -}


isComment :: String -> Bool
{- Checks if comment

   TODO!: Use a `Parser` to check for whitespace safely.
   TODO:  Implement into lexer.
 -}
isComment [] = True
isComment xs = head xs == ' '
