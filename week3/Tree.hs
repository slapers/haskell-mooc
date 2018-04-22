module Week2.Tree where

data Tree = Leaf
          | Node Int Tree Tree deriving Show

treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ t1 t2) = 1 + max (treeDepth t1) (treeDepth t2)

treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Node i t1 t2) = i + treeSum t1 + treeSum t2

isSortedTree :: Tree -> Int -> Int -> Bool
isSortedTree Leaf _ _ = True
isSortedTree (Node i t1 t2) minVal maxVal =
  (i >= minVal) && (i <= maxVal) && leftSorted && rightSorted
  where
    leftSorted = isSortedTree t1 minVal i
    rightSorted = isSortedTree t2 i maxVal

addNewMax :: Tree -> Tree
addNewMax Leaf = Node 0 Leaf Leaf
addNewMax (Node x t1 Leaf) = Node x t1 (Node (x+1) Leaf Leaf)
addNewMax (Node x t1 t2) = Node x t1 (addNewMax t2)

insertValue :: Int -> Tree -> Tree
insertValue x Leaf = Node x Leaf Leaf
insertValue x (Node y t1 t2)
  | x > y = Node y t1 (insertValue x t2)
  | otherwise = Node y (insertValue x t1) t2

toList :: Tree -> [Int]
toList Leaf = []
toList (Node x t1 t2) = toList t1 ++ [x] ++ toList t2
