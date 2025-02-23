change :: [Int] -> Int -> Int -> [Int]
change [] _ _ = error "No such position"
change (hd : tl) 0 v = (v : tl)
change (hd : tl) i v = hd : (change tl (i - 1) v)

data Dir = Fwd deriving (Show, Eq)

type Pos = [Dir]

at :: [Int] -> Pos -> [Int]
at l [] = l
at (hd : tl) (Fwd : p) = at tl p

data Crumb = Forward Int deriving (Show, Eq)

type Trail = [Crumb]

type Zipper = ([Int], Trail)

change' :: [Int] -> Pos -> Int -> [Int]
change' (hd : tl) [] v = (v : tl)
change' [] [] _ = error "Element does not exist"
change' (hd : tl) (Fwd : p) v = hd : (change' tl p v)

goFwd :: Zipper -> Zipper
goFwd ([], _) = error "Cannot go forward"
goFwd ((hd : tl), t) = (tl, (Forward hd : t))

goBwd :: Zipper -> Zipper
goBwd (_, []) = error "Cannot go backward"
goBwd (l, Forward x : t) = ((x : l), t)

change'' :: Zipper -> Int -> Zipper
change'' ([], _) v = error "At end of list"
change'' ((hd : tl), t) v = (v : tl, t)
