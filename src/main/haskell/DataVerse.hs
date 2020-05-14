module DataVerse where

import Control.Applicative

data Tree a = Node [Tree a] | Leaf a
    deriving Show

instance Functor Tree where
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node edges) = Node $ fmap (fmap f) edges

instance Applicative Tree where
  pure = Leaf
  (Leaf a2b) <*> (Leaf a) = Leaf $ a2b a
  (Leaf a2b) <*> (node @ (Node _)) = fmap a2b node
  (Node a2bEdges) <*> treeA = Node $ fmap (<*> treeA) a2bEdges

instance Monad Tree where
  return = Leaf
  (Leaf a) >>= f = f a
  (Node edges) >>= f = Node $ fmap (>>= f) edges
  
newtype DataVerseMetadata = DataVerseMetadata { name :: String }

type DV = Tree DataVerseMetadata

example1 :: DV
example1 = Leaf DataVerseMetadata { name = "my-dataset" }

example2 :: DV
example2 = Node [Leaf DataVerseMetadata { name = "my-dataset1" }, Leaf DataVerseMetadata { name = "my-dataset2" }]

example3 :: DV
example3 = Node
    [ 
      Node [Leaf DataVerseMetadata { name = "my-dataset1" }, Leaf DataVerseMetadata { name = "my-dataset2" }],
      Leaf DataVerseMetadata { name = "my-dataset3" },
      Node [Leaf DataVerseMetadata { name = "my-dataset4" }]
    ]

map1 :: Tree String
map1 = fmap name example1
