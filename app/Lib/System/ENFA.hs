module Lib.System.ENFA where
{- `Lib.System.ENFA` defines an e-NFA that rejects on
    an unknown character (as opposed to the traditional way
    of returning the location).
 -}

import qualified Data.Map as M ( Map
                               , lookup
                               , fromList
                               , empty
                               , map
                               , unionWith
                               )
import qualified Data.Set as S ( Set
                               , empty
                               , size
                               , unions
                               , union
                               , mapMonotonic
                               , fromList
                               , toList
                               , null
                               )
import Data.Maybe ( fromMaybe )

import Lib.Parse.Meta ( Parser )

type Locations = S.Set Int

data State = State { transitions :: M.Map (Maybe Char) Locations
                   , accepting   :: Bool
                   }

data Group = Group { scope      :: Locations
                   , reference  :: String
                   }

data Automaton = Automaton [State] [Group]

type Captures = M.Map Group [String]

mkNone :: String -> Automaton
mkNone expr = Automaton [ M.empty `State` False] []

mkEmpty :: String -> Automaton
mkEmpty expr = Automaton [ M.empty `State` True] []

mkSingleChar :: Char -> Automaton
mkSingleChar char = Automaton states []
   where states = [ M.fromList [(Just char, S.fromList [0])] `State` False
                  , M.empty `State` True
                  ]

concatenate :: Automaton -> Automaton -> Automaton
concatenate (Automaton a _) (Automaton b _) = Automaton states []
   where shift = S.mapMonotonic ((length a - 1) + )
         shiftState s = let new = M.map shift $ transitions s in s { transitions = new }
         b' = map shiftState b

         bridge = M.unionWith S.union $ transitions $ head b'
         connectState (State trans True) = State trans False
         connectState s = s
         a' = map connectState a

         states = a' ++ tail b' -- `b !! 0` transitions have already been moved into a'

(.&) = concatenate

kleene :: Automaton -> Automaton
kleene (Automaton a _) = Automaton [] []
   where bridge = M.unionWith S.union $ transitions $ head a
         connectState (State trans True) = State trans True
         connectState s = s
         a' = map connectState a

(.*) = kleene

union  :: Automaton -> Automaton -> Automaton
union (Automaton a _) (Automaton b _) = Automaton [] []
   where shiftB = S.mapMonotonic (length a + )
         shiftA = S.mapMonotonic (+1)
         shiftState shiftFunc s = let new = M.map shiftFunc $ transitions s in s { transitions = new }
         b' = shiftState shiftB `map` b
         a' = shiftState shiftA `map` a
         initial = State (M.fromList [(Nothing, S.fromList [1, length a])]) False


         states = [initial] ++ a' ++  b'
(.|) = union

step :: Locations -> Char -> Automaton -> Locations
step _ _ (Automaton [] _) = S.empty
step locs char (Automaton states _) = S.unions $ S.mapMonotonic retrieve locs 
    where getTransition loc input = fromMaybe S.empty $ M.lookup input $ transitions $ states !! loc
          retrieve loc = getTransition loc (Just char) `S.union` getTransition loc Nothing 

success :: Locations -> Automaton -> Bool
success _ (Automaton [] _) = False
success locs (Automaton states _) = any accept $ S.toList locs
    where accept = accepting . (states !!)

check :: Automaton -> String -> Bool
check automaton = run (automaton, initial)
  where initial = S.fromList [0]
        run (machine, locs) [] = success locs machine
        run (machine, locs) (x:xs)
                                  | S.null next = False
                                  | otherwise = run (machine, next) xs
          where next = step locs x machine
-- int((digit:[0123456789])+) >-
-- dec((digit:[0123456789])+.(digit:[0123456789])+) >-
