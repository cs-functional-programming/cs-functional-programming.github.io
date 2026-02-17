data Tree = Leaf | Node Int Tree Tree deriving (Show, Eq)

t1 :: Tree
t1 = Node 5
  (Node 3
   (Node 1 Leaf Leaf)
   (Node 7 (Node 10 Leaf Leaf) (Node 20 Leaf Leaf)))
  (Node 42 (Node 8 Leaf Leaf) Leaf)

data Direction = L | R deriving (Show, Eq, Ord)

type Position = [Direction]

go :: Position -> Tree -> Tree
go [] t = t
go (L:pos) (Node x l r) = go pos l
go (L:pos) Leaf = error "Cannot go left in leaf."
go (R:pos) (Node x l r) = go pos r
go (R:pos) Leaf = error "Cannot go right in leaf."

update :: Tree -> Int -> Tree
update Leaf _ = error "cannot update leaf"
update (Node x l r) v = Node v l r

p1 :: Position
p1 = [ L, R ]

p2 :: Position
p2 = [ R, L ]
