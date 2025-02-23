import Prelude hiding (Right, Left)

data Tree = Nil | Node Int Tree Tree deriving (Show, Eq)

t5 = Node 5 Nil Nil
t8 = Node 8 Nil Nil
t10 = Node 10 Nil Nil
t3 = Node 3 Nil Nil
t = Node 7 (Node 13 t5 t8) (Node 3 t10 t3)

data Dir = L | R deriving (Show, Eq)

type Pos = [Dir]

pos :: Pos
pos = [R, L]

atPos :: Tree -> Pos -> Int
atPos (Node x l r) [] = x
atPos (Node x l r) (L:tl) = atPos l tl
atPos (Node x l r) (R:tl) = atPos r tl

change :: Tree -> Pos -> Int -> Tree
change (Node x l r) [] v = Node v l r
change (Node x l r) (L:tl) v = Node x (change l tl v) r
change (Node x l r) (R:tl) v = Node x l (change r tl v)


goLeft1 :: (Tree, [Dir]) -> (Tree, [Dir])
goLeft1 (Node x l r, dirs) = (l, L : dirs)

goRight1 :: (Tree, [Dir]) -> (Tree, [Dir])
goRight1 (Node x l r, dirs) = (r, R : dirs)

change1 :: (Tree, [Dir]) -> Int -> (Tree, [Dir])
change1 (Node x l r, dirs) v = (Node v l r, dirs)

data Crumb = Left Int Tree | Right Int Tree deriving (Show, Eq)

goLeft2 :: (Tree, [Crumb]) -> (Tree, [Crumb])
goLeft2 (Node x l r, crumbs) = (l, Left x r : crumbs)

goRight2 :: (Tree, [Crumb]) -> (Tree, [Crumb])
goRight2 (Node x l r, crumbs) = (r, Right x l : crumbs)

-- (Tree, [Crumb]) = Zipper

goUp2 :: (Tree, [Crumb]) -> (Tree, [Crumb])
goUp2 (t, Left x r : crumbs) = (Node x t r, crumbs)
goUp2 (t, Right x l : crumbs) = (Node x l t, crumbs)

change2 :: Int -> (Tree, [Crumb]) -> (Tree, [Crumb])
change2 v (Node x l r, crumbs) = (Node v l r, crumbs)


