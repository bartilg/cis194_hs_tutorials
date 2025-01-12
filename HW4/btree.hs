data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = -1
height (Node x _ _ _) = x

insert :: a -> Tree a-> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node h ltree val rtree)
  | height ltree > height rtree =
    let newR = insert x rtree 
    in Node ((max (height newR) (height ltree)) + 1) ltree val newR
  | otherwise = 
    let newL = insert x ltree
    in Node ((max (height rtree) (height newL)) + 1) newL val rtree

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

main = do 
  print (foldTree "ABCDEFGHIJ")
