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
                   } deriving (Show, Eq)

data Group = Group { scope      :: Locations
                   , reference  :: String
                   } deriving (Show, Eq)

data Automaton = Automaton [State] [Group] deriving (Show, Eq)

type Captures = M.Map Group [String]

mkNone :: String -> Automaton
mkNone _ = Automaton [ M.empty `State` False ] []

mkEmpty :: String -> Automaton
mkEmpty _ = Automaton [ M.empty `State` True ] []

mkSingleChar :: Char -> Automaton
mkSingleChar char = Automaton states []
   where states = [ M.fromList [(Just char, S.fromList [1])] `State` False
                  , M.empty `State` True
                  ]

concatenate :: Automaton -> Automaton -> Automaton
concatenate (Automaton a _) (Automaton b _) = Automaton states []
   where shift = S.mapMonotonic ((length a - 1) + )
         shiftState s = let new = M.map shift $ transitions s in s { transitions = new }
         b' = map shiftState b

         bridge = M.unionWith S.union $ transitions $ head b'
         connectState (State trans True) = State (bridge trans) False
         connectState s = s
         a' = map connectState a

         states = a' ++ tail b' -- `b !! 0` transitions have already been moved into a'

(.&) = concatenate

kleene :: Automaton -> Automaton
kleene (Automaton a _) = Automaton a' []
   where bridge = M.unionWith S.union $ transitions $ head a
         connectState (State trans True) = State (bridge trans) True
         connectState s = s
         a' = map connectState a

(.*) = kleene

union  :: Automaton -> Automaton -> Automaton
union (Automaton a _) (Automaton b _) = Automaton states []
   where shiftA = S.mapMonotonic (+1)
         shiftB = S.mapMonotonic ((+1) . (length a + ))
         shiftState shiftFunc s = let new = M.map shiftFunc $ transitions s in s { transitions = new }
         a' = shiftState shiftA `map` a
         b' = shiftState shiftB `map` b
         initial = State (M.fromList [(Nothing, S.fromList [1, 1 + length a])]) False
         states = [initial] ++ a' ++  b'

(.|) = union


getTransition :: [State] -> Maybe Char -> Int -> Locations
getTransition states input loc = fromMaybe S.empty $ M.lookup input $ transitions $ states !! loc

grow :: Locations -> Automaton -> Locations
grow _ (Automaton [] _) = S.empty
grow locs (Automaton states _) = S.unions layers
    where addLayer = S.unions . S.mapMonotonic (getTransition states Nothing)
          layers = takeWhile (not . S.null) $ iterate addLayer locs

step :: Locations -> Char -> Automaton -> Locations
step _ _ (Automaton [] _) = S.empty
step locs char (Automaton states _) = S.unions $ S.mapMonotonic retrieve locs'
    where retrieve loc = getTransition states (Just char) loc
          locs' = S.union locs $ grow locs (Automaton states [])

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
