module Lib.Utils where

findRepeat :: Eq a => [a] -> Maybe a
findRepeat [] = Nothing
findRepeat [x] = Nothing
findRepeat (x:xs) | x == head xs = Just x
                  | otherwise = findRepeat $ tail xs
