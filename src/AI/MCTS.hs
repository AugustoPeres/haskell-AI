module AI.MCTS
  ( MCTSAgent(..)
  , selection
  , simulation
  , expansion
  , ucb
  )
  where

{--

Example usage:

:module +Games.TicTacToe AI.MCTS AI.Tree System.Random Games.Classes Utils.Utils Control.Monad.State System.Random
game = foldl step initialGame [0, 1, 3, 6, 4, 5, 7]
agent = MCTSAgent { zipper = makeZipper (Leaf (game, 0, 0)), player = currentPlayer game, numSimulations = 10 }
expandedAgent = expansion agent
selectedAgent = selection expandedAgent
simulatedAgent = selectedAgent >>= simulation
zipper expandedAgent
zipper $ fst $ runState selectedAgent (mkStdGen 42)
zipper $ fst $ runState simulatedAgent (mkStdGen 42)

|--}
import           Control.Monad.State (State, get, put)
import           System.Random       (StdGen)
import           Control.Monad (replicateM)


import           AI.Tree             (followToBottomWith, Zipper, getRootValue, addChildren, updateRootValue)
import           Games.Classes

type MCTSNode g = (g, Int, Int) -- (current game state, wins, victories)

data MCTSAgent g a p =
  MCTSAgent { zipper :: Zipper a (MCTSNode g)
            , player :: p
            , numSimulations :: Int }


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

simulation :: (AdversarialGame g a p, Eq a) => MCTSAgent g a p -> State StdGen (MCTSAgent g a p)
simulation agent =
  let z@(tree, crumbs) = zipper agent
      (gameState, _, _) = getRootValue (fst z)
      playerAgent = player agent
      simulations = numSimulations agent
      randomGames = replicateM simulations (playRandomGame gameState)
      wonGames = fmap (\games -> sum $ map f games) randomGames
      f = \game -> case winner game of
                     Just w | w == playerAgent -> 1
                     Just _                    -> -1
                     _                         -> 0
  in if winner gameState == (Just $ player agent)
     then return $ agent { zipper = (updateRootValue tree (gameState, 1, 1), crumbs)}
     else wonGames >>= (\wg -> return $ agent { zipper = (updateRootValue tree (gameState, wg, simulations), crumbs) })


expansion :: (AdversarialGame g a p, Eq a) => MCTSAgent g a p -> MCTSAgent g a p
expansion agent =
  let z = zipper agent
      (gameState, _, _) = getRootValue (fst z)
      nextActionStates = [(a, (step gameState a, 0, 0)) | a <- availableActions gameState]
  in agent {zipper = addChildren z nextActionStates}


backpropagation :: MCTSAgent g a p -> MCTSAgent g a p
backpropagation agent = undefined
  

ucb :: MCTSNode g -> MCTSNode g -> Float
ucb (_, _, p_visits) (_, c_wins, c_visits)
  | c_visits == 0 = 1 / 0
  | otherwise =
      let c = sqrt 2
          exploration = c * sqrt ((log $ fromIntegral p_visits) / fromIntegral c_visits)
          exploitation = fromIntegral c_wins / fromIntegral c_visits
      in exploration + exploitation

