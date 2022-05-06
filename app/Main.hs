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

import Lib.ReductionSystem ( TRS
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
printLang trs = do
                 print trs
                 interpret trs

query :: TRS -> String -> IO()
{- Insert rule into reduction system OR normalize provided string
 -}
query trs input = do 
                   if null input 
                    then interpret trs
                    else
                       if " -> " `isInfixOf` input 
                       then interpret $ insertRule trs $ parseRule input 
                       else
                         do 
                          putStrLn $ (++) "::: " $ show $ normalize trs $ parseTerm input
                          interpret trs

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

main :: IO ()
{- Starts `interpret` with the correct `TRS`.

   If no file is provided, `main` starts `interpret`
   with an empty `TRS`.
 -}
main = do
        args <- getArgs
        if null args 
          then interpret newTRS
          else do 
                file <- readFile $ head args
                interpret $ mkTRS $ map parseRule $ strip $ lines file
       where strip = filter (not . isComment)

