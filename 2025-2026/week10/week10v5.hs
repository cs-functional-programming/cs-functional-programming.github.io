data Tree = Empty | Nod Int Tree Tree deriving (Eq, Show)

t1 :: Tree
t1 = Nod 7 (Nod 3 (Nod 100 Empty Empty) Empty) (Nod 18 (Nod 6 Empty Empty) (Nod 5 Empty Empty))

data Crumb = L Int Tree | R Int Tree deriving (Show, Eq)

type Trace = [Crumb] 

type Zipper = (Tree, Trace)

goLeft :: Maybe Zipper -> Maybe Zipper
goLeft Nothing = Nothing
goLeft (Just (Empty, _)) = Nothing
goLeft (Just (Nod x left right, pozitie)) = Just (left, L x right : pozitie)

goRight :: Maybe Zipper -> Maybe Zipper
goRight Nothing = Nothing
goRight (Just (Empty, _)) = Nothing
goRight (Just (Nod x left right, pozitie)) = Just (right, R x left : pozitie)

getVal :: Maybe Zipper -> Maybe Int
getVal Nothing = Nothing
getVal (Just (Empty, pozitie)) = Nothing
getVal (Just (Nod x left right, pozitie)) = Just x

goUp :: Maybe Zipper -> Maybe Zipper
goUp Nothing = Nothing
goUp (Just (t, [])) = Nothing
goUp (Just (t, L x r:dirs)) = Just (Nod x t r, dirs)
goUp (Just (t, R x l:dirs)) = Just (Nod x l t, dirs)

updateZipper :: Maybe Zipper -> Int -> Maybe Zipper
updateZipper Nothing _ = Nothing
updateZipper (Just (Empty, pozitie)) newValue = Nothing
updateZipper (Just (Nod x left right, pozitie)) newValue = Just (Nod newValue left right, pozitie)

initZipper = Just (t1, [])
