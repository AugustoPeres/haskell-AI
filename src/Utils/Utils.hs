module Utils.Utils
  (
  choice
  )
  where

import System.Random

-- | Chooses a random element from a list
choice :: [a] -> StdGen -> (StdGen, a)
choice l g
  | null l    = error "Trying to take a random element from an empty list."
  | otherwise = (newGen, l !! index)
  where
    (index, newGen) = randomR (0, length l - 1) g
