module Utils.Utils
  (
  choice,
  firstElem,
  maxValuesBy
  )
  where

import System.Random

-- returns the first element to meet a condition in a list.
-- returns nothing if there is no such value
firstElem :: (a -> Bool) -> [a] -> Maybe a
firstElem _ []     = Nothing
firstElem f (x:xs) = if f x then Just x else firstElem f xs

-- | Chooses a random element from a list
choice :: [a] -> StdGen -> (StdGen, a)
choice l g
  | null l    = error "Trying to take a random element from an empty list."
  | otherwise = (newGen, l !! index)
  where
    (index, newGen) = randomR (0, length l - 1) g

-- | Returns all the elements in a list that that maximize the given function
maxValuesBy :: (Ord b) => [a] -> (a -> b) -> [a]
maxValuesBy [] _ = undefined
maxValuesBy (x:xs) f =
  go [(x, f x)] xs
  where go l [] = map fst l
        go [] _ = undefined
        go l@((_, fy):_) (a:xa)
          | fy < fa  = go [(a, fa)] xa
          | fy == fa = go ((a, fa):l) xa
          | otherwise  = go l xa
          where fa = f a
