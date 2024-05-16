import Prelude hiding (Left, Right)

data Arb = Nil | Node Int Arb Arb deriving (Show, Eq)

t1 :: Arb
t1 = Node 1 (Node 2 (Node 4 Nil Nil) (Node 5 Nil Nil)) (Node 3 Nil Nil)

f :: Arb -> Int -> Arb
f (Node x (Node y (Node o a1 a2) a3) z) v = (Node x (Node y (Node v a1 a2) a3) z)

data Dir = L | R deriving (Show, Eq)

type Poz = [Dir]

at :: Arb -> Poz -> Arb
at a [] = a
at (Node _ a1 _) (L : p) = at a1 p
at (Node _ _ a2) (R : p) = at a2 p

change :: Arb -> Poz -> Int -> Arb
change Nil [] _ = error "Nu exista valoarea veche"
change Nil _ _ = error "Nu exista pozitia"
change (Node o a1 a2) [] v = Node v a1 a2
change (Node x a1 a2) (L : p) v = (Node x (change a1 p v) a2)
change (Node x a1 a2) (R : p) v = (Node x a1 (change a2 p v))

p = [L, L]

data Crumb = Left Int Arb | Right Int Arb deriving (Show, Eq)

type Trail = [ Crumb ]

goLeft :: (Arb, Trail) -> (Arb, Trail)
goLeft (Nil, _) = error "Cannot go left in leaf"
goLeft (Node x a1 a2, t) = (a1, (Left x a2) : t)

goRight :: (Arb, Trail) -> (Arb, Trail)
goRight (Nil, _) = error "Cannot go left in leaf"
goRight (Node x a1 a2, t) = (a2, (Right x a1) : t)

goUp :: (Arb, Trail) -> (Arb, Trail)
goUp (a, []) = error "Cannot go up in root"
goUp (a, ((Left x a2) : t)) = ((Node x a a2), t)
goUp (a, ((Right x a1) : t)) = ((Node x a1 a), t)

change' :: (Arb, Trail) -> Int -> (Arb, Trail)
change' (Nil, _) _ = error "Cannot change information in leaf"
change' (Node x a1 a2, t) v = (Node v a1 a2, t)

