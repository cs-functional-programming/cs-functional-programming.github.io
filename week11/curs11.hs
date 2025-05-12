data Tree = Leaf | Node Int Tree Tree deriving (Show, Eq)

t1 :: Tree
t1 = Node 5
  (Node 3
   (Node 1 Leaf Leaf)
   (Node 7 (Node 10 Leaf Leaf) (Node 20 Leaf Leaf)))
  (Node 42 (Node 8 Leaf Leaf) Leaf)

update :: Int -> Tree -> Tree
update v (Node x (Node y n1 (Node z n2 n3)) n4) =
  (Node x (Node y n1 (Node v n2 n3)) n4)

update' :: Int -> Tree -> Tree
update' v (Node y n1 (Node x (Node z n2 n3) n4)) =
  (Node y n1 (Node x (Node v n2 n3) n4))

data Direction = L | R deriving (Show, Eq, Ord)

type Position = [Direction]

update'' :: Position -> Int -> Tree -> Tree
update'' [] v (Node x l r) = Node v l r
update'' [] v Leaf = error "Cannot update leaf."
update'' (L:pos) v (Node x l r) = Node x (update'' pos v l) r
update'' (L:pos) v Leaf = error "Cannot go left in leaf."
update'' (R:pos) v (Node x l r) = Node x l (update'' pos v r)
update'' (R:pos) v Leaf = error "Cannot go right in leaf."

p1 :: Position
p1 = [ L, R ]

p2 :: Position
p2 = [ R, L ]
