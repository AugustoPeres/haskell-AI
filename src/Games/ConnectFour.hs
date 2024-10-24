{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Games.ConnectFour
  ( ConnectFour(..)
  , Player(..)
  , Action
  , initialGame
  , isFull
  ) where

{--

Example usage:

>>> cabal build
>>> cabal repl
ghci> :module Games.ConnectFour System.Random Games.Classes Control.Monad.State
ghci> s = playRandomGame initialGame
ghci> :t s
s :: State StdGen ConnectFour
ghci> :t runState s
runState s :: StdGen -> (ConnectFour, StdGen)
ghci> fst $ runState s (mkStdGen 1)

|--}


import           Games.Classes (AdversarialGame, availableActions,
                                currentPlayer, isFinal, step, winner)


data Player = X | O deriving (Eq, Show)

type Action = Int
type Board = Int -> Int -> Maybe Player

data ConnectFour = ConnectFour
  { board :: Board
  , player :: Player
  , piecesInCol :: Int -> Int
  }

instance Show ConnectFour where
  show g = unlines $ [showRow r | r <- [5, 4..0]] ++ ["current player " ++ show (player g)]
    where
      showRow r = concat [showCell (board g c r) | c <- [0..6]]
      showCell Nothing  = "_ "
      showCell (Just X) = "X "
      showCell (Just O) = "O "

initialGame :: ConnectFour
initialGame = ConnectFour (\_ _ -> Nothing) X (\_ -> 0)

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

instance AdversarialGame ConnectFour Action Player where
  step g a =
    if not (a `elem` availableActions g)
    then g
    else g { board = newBoard
           , player = nextPlayer cPlayer
           , piecesInCol = newPiecesInCol}
    where newBoard c r = if c == a && r == row
                         then Just cPlayer
                         else (board g) c r
          newPiecesInCol c = if c == a
                             then (piecesInCol g c) + 1
                             else (piecesInCol g c)
          row = (piecesInCol g) a
          cPlayer = player g

  availableActions g = filter (\x -> (piecesInCol g) x <= 5) [0..6]

  currentPlayer = player

  isFinal g = hasWinningCombination g X ||
              hasWinningCombination g O ||
              isFull g

  winner g
    | hasWinningCombination g X = Just X
    | hasWinningCombination g O = Just O
    | otherwise                 = Nothing


hasWinningCombination :: ConnectFour -> Player -> Bool
hasWinningCombination g p =
  any (isWinningCombination p g) diag1WinningCombinations ||
  any (isWinningCombination p g) diag2WinningCombinations ||
  any (isWinningCombination p g) verticalWinningCombinations ||
  any (isWinningCombination p g) horizontalWinningCombinations


isWinningCombination :: Player -> ConnectFour -> [(Int, Int)] -> Bool
isWinningCombination p (ConnectFour b _ _) combs = all (\(x, y) -> b x y == Just p) combs

isFull :: ConnectFour -> Bool
isFull g = all (\(x, y) -> (board g) x y /= Nothing) [(x, y) | x <- [0..6], y <- [0..5]]

diag1WinningCombinations :: [[(Int, Int)]]
diag1WinningCombinations = [[(x + i, y + i) | i <- [0..3]] | x <- [0..3], y <- [0..2]]

diag2WinningCombinations :: [[(Int, Int)]]
diag2WinningCombinations = [[(x - i, y + i) | i <- [0..3]] | x <- [3..6], y <- [0..2]]

verticalWinningCombinations :: [[(Int, Int)]]
verticalWinningCombinations = [[(x + i, y) | i <- [0..3]] | x <- [0..3], y <- [0..5]]

horizontalWinningCombinations :: [[(Int, Int)]]
horizontalWinningCombinations = [[(x , y + i) | i <- [0..3]] | x <- [0..6], y <- [0..2]]
          

