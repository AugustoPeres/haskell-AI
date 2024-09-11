module AI.Tree
  ( Tree
  )

import           Utils.Utils      (firstElem, maxValuesBy, choice)

  
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
