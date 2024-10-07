module AI.MCTS
  ( MCTSAgent(..)
  , selection
  )
  where

import           Control.Monad.State (State, get, put)
import           System.Random       (StdGen)

import           AI.Tree             (followToBottomWith, Zipper)
import           Games.Classes

type MCTSNode g = (g, Int, Int) -- (current game state, wins, victories)

data MCTSAgent g a p =
  MCTSAgent { zipper :: Zipper a (MCTSNode g)
            , player :: p }


takeAction :: (AdversarialGame g a p, Eq p, Ord g) => g -> State StdGen a
takeAction _ = undefined


selection :: (AdversarialGame g a p, Eq a) => MCTSAgent g a p -> State StdGen (MCTSAgent g a p)
selection agent =
  followToBottomWith (zipper agent) f >>= (\z -> return $ agent { zipper = z })
  where f parent@(gameState, pWins, pVisits) child@(gameState', cWins, cVisits) =
          if currentPlayer gameState == player agent
          then ucb parent child
          -- Revert the UCB choice for the player. Only works for 2 player games.
          else ucb (gameState, pVisits - pWins, pVisits) (gameState', cVisits - cWins, cWins)
  

ucb :: MCTSNode g -> MCTSNode g -> Float
ucb (_, _, p_visits) (_, c_wins, c_visits) =
  exploration + exploitation
  where c = sqrt 2
        exploration = c * sqrt ((log $ fromIntegral p_visits) / fromIntegral c_visits)
        exploitation = fromIntegral c_wins / fromIntegral c_visits

