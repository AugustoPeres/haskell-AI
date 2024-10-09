module AI.Tree
  ( Tree(..)
  , Zipper
  , makeZipper
  , followDirection
  , getRootValue
  , goUpWith
  , goToTopWith
  , followToBottomWith
  , addChildren
  , updateRootValue
  ) where

import           Utils.Utils      (firstElem, maxValuesBy, choice)
import           Control.Monad.State (State, get, put)
import           System.Random       (StdGen)

  
data Tree crumb value = Leaf value | Node value [(crumb, Tree crumb value)] deriving (Eq, Show, Ord)

instance Functor (Tree crumb) where
  fmap f (Leaf value)          = Leaf (f value)
  fmap f (Node value children) = Node (f value) [(crumb, fmap f t) | (crumb, t) <- children]


data Crumb crumb value = Crumb value crumb [(crumb, Tree crumb value)] deriving (Eq, Show, Ord)

type Zipper crumb value = (Tree crumb value, [Crumb crumb value])


makeZipper :: Tree crumb value -> Zipper crumb value
makeZipper tree = (tree, [])

followDirection :: (Eq crumb) => Zipper crumb value -> crumb -> Zipper crumb value
followDirection (l@(Leaf _), c) _ = (l, c)
followDirection (Node val children, c) crumb =
  case firstElem (\(c', _) -> c' == crumb) children of
    Nothing           -> error "nothing follows this crumb"
    Just (_, subTree) -> (subTree, Crumb val crumb (filter (\(c', _) -> c' /= crumb) children) : c) 

goUpWith :: Zipper crumb value -> (value -> value -> value) -> Zipper crumb value
goUpWith z@(_, []) _ = z
goUpWith (tree, (Crumb value crum children):xs) f =
  (Node resultingValue ((crum, tree):children), xs) 
  where resultingValue = f (getRootValue tree ) value 

goToTopWith :: Zipper crumb value -> (value -> value -> value) -> Zipper crumb value
goToTopWith z@(_, []) _ = z
goToTopWith z f         = goToTopWith (goUpWith z f) f

addChildren :: Zipper crumb value -> [(crumb, value)] -> Zipper crumb value
addChildren z [] = z
addChildren ((Leaf value), c) newChildren = (Node value [(crumb, Leaf v) | (crumb, v) <- newChildren], c)
addChildren ((Node value children), c) newChildren = (Node value (children ++ [(crumb, Leaf v) | (crumb, v) <- newChildren]), c)

followToBottomWith :: (Eq crumb, Ord b) => Zipper crumb value -> (value -> value -> b) -> State StdGen (Zipper crumb value)
followToBottomWith (l@(Leaf _), c) _ = return (l, c)
followToBottomWith z@(Node v children, _) f =
  get >>= (\gen ->
             let maxChildren = maxValuesBy children ((f v) . getRootValue . snd)
                 (g', direction) = choice (map fst maxChildren) gen
             in put g' >> followToBottomWith (followDirection z direction) f)

getRootValue :: Tree crumb value -> value
getRootValue (Leaf v)   = v
getRootValue (Node v _) = v


updateRootValue :: Tree crumb value -> value -> Tree crumb value
updateRootValue (Leaf _) v  = Leaf v
updateRootValue (Node _ c) v = Node v c
