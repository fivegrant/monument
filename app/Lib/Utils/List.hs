module Lib.Utils.List where
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


changeElement :: [a] -> Int -> a -> [a]
{- Change element at the given index.

   Note: I should probably be using `lens`.
 -}
changeElement xs i new = left ++ [new] ++ tail right
    where (left, right) = splitAt i xs

compress :: [(Int, a)] -> [[a]]
{- Reduce ordered pairs into a list of lists ordered
   by the index provided by the first element in the pair.

   TODO: Reduce time complexity.
 -}
compress pairs = foldl collect initialList pairs
         where size = (+1) $ maximum $ map fst pairs
               initialList = replicate size []
               newList xs pair = xs !! fst pair ++ [snd pair]
               collect xs pair = changeElement xs (fst pair) (newList xs pair)
