data Tree = Empty | Nod Int Tree Tree deriving (Eq, Show)

t1 :: Tree
t1 = Nod 7 (Nod 3 (Nod 100 Empty Empty) Empty) (Nod 18 (Nod 6 Empty Empty) (Nod 5 Empty Empty))

data Directie = L | R deriving (Show, Eq)

type Pozitie = [Directie] 

type Zipper = (Tree, Pozitie)

goLeft :: Zipper -> Zipper
goLeft (Empty, _) = error "nu pot merge la stanga"
goLeft (Nod x left right, pozitie) = (left, L : pozitie)

goRight :: Zipper -> Zipper
goRight (Empty, _) = error "nu pot merge la dreapta"
goRight (Nod x left right, pozitie) = (right, R : pozitie)

getVal :: Zipper -> Int
getVal (Empty, pozitie) = error "nu pot accesa valoarea"
getVal (Nod x left right, pozitie) = x

-- cum scriu goUp?
-- goUp :: Zipper -> Zipper
-- goUp (t, pozitie) = (t, take (length pozitie - 1) pozitie)

-- updateZipper :: Zipper -> Int -> Zipper
-- updateZipper (t, pozitie) newValue = (changeValue t pozitie newValue, pozitie)

initZipper = (t1, [])
