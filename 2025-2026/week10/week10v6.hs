data Tree = Empty | Nod Int Tree Tree deriving (Eq, Show)

t1 :: Tree
t1 = Nod 7 (Nod 3 (Nod 100 Empty Empty) Empty) (Nod 18 (Nod 6 Empty Empty) (Nod 5 Empty Empty))

data Crumb = L Int Tree | R Int Tree deriving (Show, Eq)

type Trace = [Crumb] 

type Zipper = (Tree, Trace)

goLeft :: Zipper -> Maybe Zipper
goLeft (Empty, _) = Nothing
goLeft (Nod x left right, pozitie) = Just (left, L x right : pozitie)

goRight :: Zipper -> Maybe Zipper
goRight (Empty, _) = Nothing
goRight (Nod x left right, pozitie) = Just (right, R x left : pozitie)

getVal :: Zipper -> Maybe Int
getVal (Empty, pozitie) = Nothing
getVal (Nod x left right, pozitie) = Just x

goUp :: Zipper -> Maybe Zipper
goUp (t, []) = Nothing
goUp (t, L x r:dirs) = Just (Nod x t r, dirs)
goUp (t, R x l:dirs) = Just (Nod x l t, dirs)

updateZipper :: Zipper -> Int -> Maybe Zipper
updateZipper (Empty, pozitie) newValue = Nothing
updateZipper (Nod x left right, pozitie) newValue = Just (Nod newValue left right, pozitie)

update :: Int -> Zipper -> Maybe Zipper
update = flip updateZipper

initZipper = (t1, [])

process :: Zipper -> Maybe Zipper
process z1 = do z2 <- goLeft z1
                z3 <- goLeft z2
                z3' <- updateZipper z3 1
                z4 <- goUp z3'
                z5 <- goUp z4
                return z5
process' :: Zipper -> Maybe Zipper
process' z1 = Just z1 >>= goLeft >>= goLeft >>= update 1 >>= goUp >>= goUp

process'' :: Zipper -> Maybe Zipper
process'' z1 = Just z1 >>= goLeft >>= goLeft >>= update 1 >>= goUp >>= update 13 >>= goUp

process''' :: Zipper -> Maybe Zipper
process''' z1 = Just z1 >>= goLeft >>= goLeft >>= goLeft >>= goLeft >>= update 1 >>= goUp >>= update 13 >>= goUp
