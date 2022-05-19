module Lib.System.ENFA where

import Data.Map ( Map
                , lookup
                )
import Lib.Parse.Meta ( Parser )

type Location = Int

data State = State { transitions :: Map (Maybe Char) [Location]
                   , accepting   :: Bool
                   }

data Group = Group { scope      :: [Location]
                   , reference  :: String
                   }

data Automaton = Automaton [State] [Group]

type Captures = Map Group [String]


mkAutomaton :: String -> Automaton
mkAutomaton expr = Automaton [] []

step :: Location -> Char -> Automaton -> [Location]
step _ _ (Automaton [] _) = [] 
step loc input (Automaton states _)
    | loc >= length states || loc < 0 = []
    | otherwise = Data.Map.lookup input $ transitions $ states !! loc

success :: Location -> Automaton -> Bool
success _ (Automaton [] _) = False
success loc (Automaton states _) 
    | loc >= length states || loc < 0 = False
    | otherwise = accepting $ states !! loc

check :: Automaton -> String -> Bool
check automaton = run (automaton, 0)
  where run (machine, loc) [] = success loc machine
        run (machine, loc) (x:xs) = case step loc x machine of
                                     Just next -> run (machine, next) xs
                                     _ -> False

-- int((digit:[0123456789])+) >-
-- dec((digit:[0123456789])+.(digit:[0123456789])+) >-
