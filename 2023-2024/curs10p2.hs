data Lista = Nil | Cons Int Lista deriving (Show, Eq)

l = Cons 3 (Cons 8 (Cons 13 (Cons 7 Nil)))

-- 13 este la pozitia 2

pos = 2

l' = change l pos 11

change :: Lista -> Int -> Int -> Lista
change (Cons v tl) 0 v' = (Cons v' tl)
change (Cons v tl) poz v' = (Cons v (change tl (poz - 1) v'))

data Dir = Fwd deriving (Show, Eq)

type Pos = [Dir]

atPos :: Lista -> Pos -> Int
atPos (Cons v tl) [] = v
atPos (Cons v tl) (Fwd:dirs) = atPos tl dirs

data Crumb = Go Int deriving (Show, Eq)

goFwd :: (Lista, [Crumb]) -> (Lista, [Crumb])
goFwd (Cons v tl, crumbs) = (tl, Go v : crumbs)

chg :: Int -> (Lista, [Crumb]) -> (Lista, [Crumb])
chg v' (Cons v tl, crumbs) = (Cons v' tl, crumbs)

goBwd :: (Lista, [Crumb]) -> (Lista, [Crumb])
goBwd (l, Go v : crumbs) = (Cons v l, crumbs)

type Zipper = (Lista, [Crumb])

(-:) :: Zipper -> (Zipper -> Zipper) -> Zipper
z -: f = f z

