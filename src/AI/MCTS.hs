module AI.MCTS
  ( MCTSAgent(..)
  , selection
  , simulation
  , expansion
  , backpropagation
  , ucb
  , mctsIteration
  , takeAction
  )
  where

{--

Example usage:

:module +Games.TicTacToe AI.MCTS AI.Tree System.Random Games.Classes Utils.Utils Control.Monad.State System.Random
game = foldl step initialGame [0, 1, 3, 6, 4, 5]
agent = MCTSAgent { zipper = makeZipper (Leaf (game, 0, 0)), player = currentPlayer game, numSimulations = 10 }
expandedAgent = expansion agent
selectedAgent = selection expandedAgent
simulatedAgent = selectedAgent >>= simulation
zipper expandedAgent
zipper $ fst $ runState selectedAgent (mkStdGen 42)
zipper $ fst $ runState simulatedAgent (mkStdGen 42)

|--}
import           Control.Monad.State (State, get, put, runState)
import           System.Random       (StdGen)
import           Control.Monad (replicateM)
import           Data.List ( (\\) )
import Control.Monad (foldM)


import           AI.Tree             (followToBottomWith, Zipper, getRootValue, addChildren, updateRootValue, goToTopWith, getChildren, followDirection, makeTree, makeZipper)
import           Games.Classes
import Utils.Utils (choice, maxValuesBy)

type MCTSNode g = (g, Int, Int) -- (current game state, wins, victories)

data MCTSAgent g a p =
  MCTSAgent { zipper :: Zipper a (MCTSNode g)
            , player :: p
            , numSimulations :: Int } deriving (Show)


takeAction :: (AdversarialGame g a p, Eq p, Eq a) => g -> Int -> State StdGen a
takeAction gameState numIterations =
  get >>= (\gen ->
          let agent = MCTSAgent { zipper = makeZipper (makeTree (gameState, 0, 0)),
                                  player = currentPlayer gameState,
                                  numSimulations = 10 }
              iteratedAgent = foldM (\ag _ -> mctsIteration ag) agent [1..numIterations]
              (agent', newGen) = runState iteratedAgent gen
              (tree, _) = zipper agent'
              f = (\(_, w, n) -> fromIntegral w / fromIntegral n) . getRootValue . snd
              maxChildren = maxValuesBy (getChildren tree) f
              (newGen', action) = choice (map fst maxChildren) newGen
          in put newGen' >> return action)

mctsIteration :: (AdversarialGame g a p, Eq a) => MCTSAgent g a p -> State StdGen (MCTSAgent g a p)
mctsIteration agent = (selection agent) >>= (return . expansion) >>= simulation >>= (return . backpropagation)

selection :: (AdversarialGame g a p, Eq a) => MCTSAgent g a p -> State StdGen (MCTSAgent g a p)
selection agent =
  followToBottomWith (zipperAgent) f stoppage >>= (\z -> return $ agent { zipper = z })
  where f parent@(gameState, pWins, pVisits) child@(gameState', cWins, cVisits) =
          if currentPlayer gameState == player agent
          then ucb parent child
          -- Revert the UCB choice for the player. Only works for 2 player games.
          else ucb (gameState, pVisits - pWins, pVisits) (gameState', cVisits - cWins, cWins)
        zipperAgent@(tree, _) = zipper agent
        stoppage (gameState, _, _) = length (availableActions gameState) /= length (getChildren tree)

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
  let z@(tree, _) = zipper agent
      (gameState, _, _) = getRootValue (fst z)
  in case availableActions gameState \\ map fst (getChildren tree) of
       []   -> agent
       a:_ -> agent {zipper = followDirection (addChildren z [(a, (step gameState a, 0, 0))]) a}

backpropagation :: MCTSAgent g a p -> MCTSAgent g a p
backpropagation agent =
  let z@(tree, _) = zipper agent
      (_, wins, sims) = getRootValue tree
      f = \(_, _, _) (gameState, a, b) -> (gameState, a + wins, b + sims)
      newZipper = goToTopWith z f
  in agent { zipper = newZipper }

ucb :: MCTSNode g -> MCTSNode g -> Float
ucb (_, _, p_visits) (_, c_wins, c_visits)
  | c_visits == 0 = 1 / 0
  | otherwise =
      let c = sqrt 2
          exploration = c * sqrt ((log $ fromIntegral p_visits) / fromIntegral c_visits)
          exploitation = fromIntegral c_wins / fromIntegral c_visits
      in exploration + exploitation

