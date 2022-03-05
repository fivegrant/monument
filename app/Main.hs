module Main where

import Control.Monad (when)
import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering, LineBuffering) )
import System.Environment (getArgs)

import Lib.ReductionSystem
import Lib.Term


interpret :: TRS -> IO ()
interpret trs = do
        hSetBuffering stdout NoBuffering
        putStr "=> "
        input <- getLine
        hSetBuffering stdout LineBuffering
        putStrLn $ "::: " ++ input
        --if input /= "\\quit" then interpret trs else return ()
        when (input /= "\\quit") (interpret trs)

main :: IO ()
main = do
        args <- getArgs
        if null args 
          then interpret newTRS
          else interpret newTRS

