module NTree where

data NTree a = Node [NTree a] | Leaf a deriving (Eq)

wf :: NTree a -> Bool
wf = undefined

render :: Show a => NTree a -> String
render = const "not implemented"

instance Show a => Show (NTree a) where
  show = render

height :: NTree a -> Int
height = undefined

balanced :: NTree a -> Bool
balanced = undefined

instance Functor NTree where
  fmap = undefined