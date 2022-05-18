module Main where
{- `Main` builds an executable which runs the interpreter.
 -}

import Data.List ( isInfixOf )
import Control.Monad ( when )
import System.IO ( hSetBuffering
                 , stdout
                 , BufferMode ( NoBuffering
                              , LineBuffering
                              )
                 )
import System.Environment ( getArgs )

import Lib.System.TRS ( TRS
                      , newTRS
                      , mkTRS
                      , insertRule
                      , normalize
                      )

import Lib.Parse.Parser ( parseRule
                        , parseTerm
                        , isComment
                        )

printLang :: TRS -> IO()
{- Print the entire active reduction system to output
 -}
printLang trs = print trs >> interpret trs

query :: TRS -> String -> IO()
{- Insert rule into reduction system OR normalize provided string
 -}
query trs input
                | null input  = interpret trs
                |" -> " `isInfixOf` input  = interpret $ insertRule trs $ parseRule input 
                | otherwise = printResult input >> interpret trs
  where printResult = putStrLn . (++) "::: " . show . normalize trs . parseTerm

interpret :: TRS -> IO ()
{- Handles user input and calls corresponding functions.

   `interpret` is recursively called to ensure the interpreter is active.
 -}
interpret trs = do
        hSetBuffering stdout NoBuffering
        putStr "=> "
        input <- getLine
        hSetBuffering stdout LineBuffering
        when (input /= "\\quit") (if input == "\\print" 
                                  then printLang trs
                                  else query trs input)

load :: [String] -> IO ()
{- Starts `interpret` with the correct `TRS`.

   If no file is provided, `main` starts `interpret`
   with an empty `TRS`.
 -}
load [] = interpret newTRS
load (arg:_) = readFile arg >>= buildTRS
       where strip = filter (not . isComment)
             buildTRS = interpret . mkTRS . map parseRule . strip . lines

main :: IO ()
{- Starts the interpreter.
 -}
main = getArgs >>= load

