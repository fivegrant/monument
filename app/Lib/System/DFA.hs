module Lib.System.DFA where

import Data.Map ( Map )
import Lib.Parse.Meta ( Parser )

type State = Int

data State = State { transitions :: Map Char Int
                   , accepting   :: Bool
                   }

type Automaton = [State]

type Location = Int

data Group = Group { scope      :: [Location]
                   , reference  :: String
                   }

type Captures = Map Group [String]

mkAutomaton expr = "(?):"

step :: Location -> Char -> Automaton -> Maybe Location
step _ _ [] = Nothing
step loc input automaton
    | loc >= length automaton || loc < 0 = Nothing
    | otherwise = lookup input $ transitions $ Automaton !! loc

success :: Location -> Automaton -> Bool
success _ [] = False
success loc automaton 
    | loc >= length automaton || loc < 0 = False
    | otherwise = accepting $ Automaton !! loc

check :: Automaton -> String -> Bool
check automaton input = run (automation, 0) input
  where run (machine, loc) [] = success loc machine
        run (machine, loc) x:xs = case (step loc machine x) in 
	                             Just next = run (machine, next) xs
				     _ = False

-- int((digit:[0123456789])+) >-
-- dec((digit:[0123456789])+.(digit:[0123456789])+) >-
