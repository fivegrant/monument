module Main where

import Control.Monad (when)
import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering, LineBuffering) )
import System.Environment (getArgs)

import Lib.ReductionSystem
import Lib.Term
import Lib.Parser

import Data.Set (fromList)

printLang :: TRS -> IO()
printLang trs = do
                 print trs
                 interpret trs

interpret :: TRS -> IO ()
interpret trs = do
        hSetBuffering stdout NoBuffering
        putStr "=> "
        input <- getLine
        hSetBuffering stdout LineBuffering
        putStrLn $ "::: " ++ input
        when (input /= "\\quit") (if input /= "\\print" 
                                  then interpret trs 
                                  else printLang trs)

main :: IO ()
main = do
        args <- getArgs
        if null args 
          then interpret newTRS
          else do 
                file <- readFile $ head args
                interpret $ generateTRS $ map parseRule $ lines file

