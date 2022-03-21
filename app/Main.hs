module Main where

import Control.Monad (when)
import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering, LineBuffering) )
import System.Environment (getArgs)

import Lib.ReductionSystem
import Lib.Term
import Lib.Parser

printLang :: TRS -> IO()
printLang trs = do
                 print trs
                 interpret trs

query :: TRS -> String -> IO()
query trs input = do 
                   if null input 
                    then interpret trs
                    else
                       do 
                        putStrLn $ (++) "::: " $ show $ normalize trs $ parseTerm input
                        interpret trs

interpret :: TRS -> IO ()
interpret trs = do
        hSetBuffering stdout NoBuffering
        putStr "=> "
        input <- getLine
        hSetBuffering stdout LineBuffering
        when (input /= "\\quit") (if input == "\\print" 
                                  then printLang trs
                                  else query trs input)

main :: IO ()
main = do
        args <- getArgs
        if null args 
          then interpret newTRS
          else do 
                file <- readFile $ head args
                interpret $ generateTRS $ map parseRule $ lines file

