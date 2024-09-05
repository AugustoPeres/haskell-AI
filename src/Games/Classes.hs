{-# LANGUAGE FunctionalDependencies #-}
module Games.Classes(
  AdversarialGame,
  step,
  availableActions,
  isFinal,
  currentPlayer,
  getRandomAction,
  playRandomAction,
  playRandomGame,
  winner,) where

import Control.Monad.State (State, get, put)
import System.Random (StdGen, split)

import           Utils.Utils (choice)

class (Eq p) => AdversarialGame g a p | g -> a p where
  step :: g -> a -> g
  availableActions :: g -> [a]
  currentPlayer :: g -> p
  isFinal :: g -> Bool
  winner :: g -> Maybe p

  getRandomAction :: StdGen -> g -> Maybe a
  getRandomAction gen game =
    if null $ availableActions game
      then Nothing
      else Just action
    where (_, action) = choice (availableActions game) gen

  playRandomAction :: g -> State StdGen g
  playRandomAction game = do
    gen <- get
    case getRandomAction gen game of
      Nothing -> return game
      Just action -> do
        let newGame = step game action
        let (gen1, _) = split gen
        put gen1
        return newGame
    
  playRandomGame :: g -> State StdGen g
  playRandomGame game = do
    if isFinal game
      then return game
      else do
        newGame <- playRandomAction game
        playRandomGame newGame
    
    
