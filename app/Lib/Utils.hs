module Lib.Utils where
{- `Lib.Utils` provides project wide helper functions.

   Functions that do not apply uniquely to rewriting system
   problems are kept in `Lib.Utils`.
 -}
findRepeat :: Eq a => [a] -> Maybe a
{- Returns the first element that repeats in the list.

   If `x` matches the follwing element, `Just x` is returned.
   If there is not match, `Nothing`.
 -}
findRepeat [] = Nothing
findRepeat [x] = Nothing
findRepeat (x:xs) | x == head xs = Just x
                  | otherwise = findRepeat xs

