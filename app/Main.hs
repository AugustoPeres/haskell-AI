{--
cabal build
cabal run
|--}
module Main where

import           Control.Monad.State (runState)
import           System.Random       (StdGen, mkStdGen)

import           Games.Classes (AdversarialGame, isFinal, availableActions, step, winner)
import AI.MCTS (takeAction)
import Games.ConnectFour (initialGame)



gameLoop :: (AdversarialGame g a p, Show a, Show p, Read a, Eq a, Show g) => g -> StdGen -> IO ()
gameLoop game gen
  | isFinal game = case winner game of
      Just p  -> putStrLn $ "Game Over! The winner is: " ++ show p ++ "\n" ++ show game
      Nothing -> putStrLn "Game Over! It's a draw."
  | otherwise = do
      print game
      putStrLn "Available actions: "
      print (availableActions game)
      putStrLn "Enter your action (as an integer): "
      input <- getLine
      let action = read input
          game' = step game action
      print game'
      aiTurn game' gen

aiTurn :: (AdversarialGame g a p, Show a, Show p, Read a, Eq a, Show g) => g -> StdGen -> IO ()
aiTurn game gen
  | isFinal game = case winner game of
      Just p  -> putStrLn $ "Game Over! The winner is: " ++ show p ++ "\n" ++ show game
      Nothing -> putStrLn "Game Over! It's a draw."
  | otherwise = do
      putStrLn "AI thinking..."
      let (aiAction, newGen) = runState (takeAction game 500) gen
      putStrLn $ "AI chose action: " ++ show aiAction
      gameLoop (step game aiAction) newGen  -- Back to human's turn

main :: IO ()
main = do
  let seed = mkStdGen 1
  gameLoop initialGame seed
  
