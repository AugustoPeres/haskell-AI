{-# LANGUAGE FunctionalDependencies #-}

module Games.Classes
  ( AdversarialGame
  , step
  , availableActions
  , isFinal
  , currentPlayer
  , getRandomAction
  , playRandomAction
  , playRandomGame
  , winner
  ) where

import           Control.Monad.State (State, get, put)
import           System.Random       (StdGen)

import           Utils.Utils         (choice)

class (Eq p) =>
      AdversarialGame g a p
  | g -> a p
  where
  step :: g -> a -> g
  availableActions :: g -> [a]
  currentPlayer :: g -> p
  isFinal :: g -> Bool
  winner :: g -> Maybe p
  getRandomAction :: g -> State StdGen (Maybe a)
  getRandomAction game =
    if null actions
      then return Nothing
      else get >>= \gen ->
             let (newGen, action) = choice actions gen
              in put newGen >> return (Just action)
    where
      actions = availableActions game
  playRandomAction :: g -> State StdGen g
  playRandomAction game = do
    getRandomAction game
      >>= (\a ->
             case a of
               Nothing     -> return game
               Just action -> return $ step game action)
  playRandomGame :: g -> State StdGen g
  playRandomGame game =
    playRandomAction game >>=
    (\g ->
       case isFinal g of
         True -> return g
         _    -> playRandomGame g)
