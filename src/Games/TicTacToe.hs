{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Games.TicTacToe (
    TicTacToe,
    Player(..),
    Action,
    initialGame
) where

{--

Example usage:

>>> cabal build
>>> cabal repl
ghci> :module Games.TicTacToe System.Random Games.Classes Control.Monad.State
ghci> s = playRandomGame initialGame
ghci> :t s
s :: State StdGen TicTacToe
ghci> :t runState s
runState s :: StdGen -> (TicTacToe, StdGen)
ghci> fst $ runState s (mkStdGen 1)
TicTacToe {board = [Just O,Just X,Just O,Just O,Just X,Just X,Just O,Nothing,Just X], player = X}

|--}

import Games.Classes (AdversarialGame, step, availableActions, isFinal, winner, currentPlayer)

data Player = X | O deriving (Eq, Show)
type Action = Int
type Board = [Maybe Player]

data TicTacToe = TicTacToe {
    board :: Board,
    player :: Player
} deriving (Eq, Show)

initialGame :: TicTacToe
initialGame = TicTacToe (replicate 9 Nothing) X

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

instance AdversarialGame TicTacToe Action Player where
    step (TicTacToe b p) action =
      case b !! action of
        Nothing -> TicTacToe newBoard (nextPlayer p)
        Just _ -> TicTacToe b p
      where newBoard = [f i mark | (i, mark) <- zip [0..] b]
            f i mark = if i == action then Just p else mark

    availableActions (TicTacToe b _) = [i | (i, Nothing) <- zip [0..8] b]

    currentPlayer = player

    isFinal game =
      any (\x -> isWinningCombination x (board game)) [X, O]

    winner (TicTacToe b _) =
      if isWinningCombination X b
        then Just X
        else if isWinningCombination O b
                then Just O
                else Nothing

isWinningCombination :: Player -> Board -> Bool
isWinningCombination p b = any (all (== Just p)) winningIndices
  where
    winningIndices = [
        [b !! 0, b !! 1, b !! 2],
        [b !! 3, b !! 4, b !! 5],
        [b !! 6, b !! 7, b !! 8],
        [b !! 0, b !! 3, b !! 6],
        [b !! 1, b !! 4, b !! 7],
        [b !! 2, b !! 5, b !! 8],
        [b !! 0, b !! 4, b !! 8],
        [b !! 2, b !! 4, b !! 6]
      ]
    
