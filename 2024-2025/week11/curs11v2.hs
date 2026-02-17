data Tree = Leaf | Node Int Tree Tree deriving (Show, Eq, Ord)

t1 :: Tree
t1 = Node 5
  (Node 3
   (Node 1 Leaf Leaf)
   (Node 7 (Node 10 Leaf Leaf) (Node 20 Leaf Leaf)))
  (Node 42 (Node 8 Leaf Leaf) Leaf)

data Crumb = L Int Tree | R Int Tree deriving (Show, Eq, Ord)

type Trail = [Crumb]

type Zipper = (Tree, Trail)

-- conceptually: Zipper is a Tree + a position into the tree

z1 :: Zipper
z1 = (t1, [])

goLeft :: Zipper -> Zipper
goLeft (Leaf, _) = error "Cannot go left in leaf."
goLeft (Node x l r, trail) = (l, L x r : trail)

goRight :: Zipper -> Zipper
goRight (Leaf, _) = error "Cannot go right in leaf."
goRight (Node x l r, trail) = (r, R x l : trail)

update :: Int -> Zipper -> Zipper
update _ (Leaf, _) = error "Cannot update leaf."
update v (Node x l r, trail) = (Node v l r, trail)

goUp :: Zipper -> Zipper
goUp (t, []) = error "Cannot go back (no crumbs)."
goUp (t, L x r : trail) = (Node x t r, trail)
goUp (t, R x l : trail) = (Node x l t, trail)






-- go :: Position -> Tree -> Tree
-- go [] t = t
-- go (L:pos) (Node x l r) = go pos l
-- go (L:pos) Leaf = error "Cannot go left in leaf."
-- go (R:pos) (Node x l r) = go pos r
-- go (R:pos) Leaf = error "Cannot go right in leaf."

-- update :: Tree -> Int -> Tree
-- update Leaf _ = error "cannot update leaf"
-- update (Node x l r) v = Node v l r

-- p1 :: Position
-- p1 = [ L, R ]

-- p2 :: Position
-- p2 = [ R, L ]
