module Trees
( Tree (..)
, singleton
, treeInsert
, treeElem
) where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node n left right)
    | x == n = Node n left right
    | x < n  = Node n (treeInsert x left) right
    | x > n  = Node n left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node n left right)
    | x == n = True
    | x < n  = treeElem x left
    | x > n  = treeElem x right
