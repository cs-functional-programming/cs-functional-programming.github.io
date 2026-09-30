data Tree = Empty | Nod Int Tree Tree deriving (Eq, Show)

t1 :: Tree
t1 = Nod 7 (Nod 3 (Nod 100 Empty Empty) Empty) (Nod 18 (Nod 6 Empty Empty) (Nod 5 Empty Empty))

data Crumb = L Int Tree | R Int Tree deriving (Show, Eq)

type Trace = [Crumb] 

type Zipper = (Tree, Trace)

goLeft :: Zipper -> Zipper
goLeft (Empty, _) = error "nu pot merge la stanga"
goLeft (Nod x left right, pozitie) = (left, L x right : pozitie)

goRight :: Zipper -> Zipper
goRight (Empty, _) = error "nu pot merge la dreapta"
goRight (Nod x left right, pozitie) = (right, R x left : pozitie)

getVal :: Zipper -> Int
getVal (Empty, pozitie) = error "nu pot accesa valoarea"
getVal (Nod x left right, pozitie) = x

goUp :: Zipper -> Zipper
goUp (t, L x r:dirs) = (Nod x t r, dirs)
goUp (t, R x l:dirs) = (Nod x l t, dirs)

updateZipper :: Zipper -> Int -> Zipper
updateZipper (Empty, pozitie) newValue = error "cannot update information in empty tree"
updateZipper (Nod x left right, pozitie) newValue = (Nod newValue left right, pozitie)


initZipper = (t1, [])
