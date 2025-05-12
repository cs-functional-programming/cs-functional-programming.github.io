l1 :: [Int]
l1 = [ 3, 7, 4, 6, 42 ]

update :: Int -> Int -> [Int] -> [Int]
update _ v [] = error "Cannot update empty list."
update 0 v (hd:tl) = v:tl
update p v (hd:tl) = hd : (update (p - 1) v tl)

-- update 3 10 l1 = [ 3, 7, 4, 10, 42 ]

-- data Direction = Fwd deriving (Show, Eq, Ord)

-- type Position = [Direction]

-- update' :: Position -> Int -> [Int] -> [Int]
-- update' _ v [] = error "Cannot update empty list."
-- update' [] v (hd:tl) = v:tl
-- update' (Fwd : p) v (hd:tl) = hd : (update' p v tl)

data Crumb = Fwd Int deriving (Show, Eq, Ord)

type Trail = [Crumb]

type Zipper = ([Int], Trail)

goFwd :: Zipper -> Zipper
goFwd ([], _) = error "Cannot go forward in empty list."
goFwd (hd:tl, trail) = (tl, Fwd hd : trail)

update'' :: Int -> Zipper -> Zipper
update'' _ ([], _) = error "Cannot update empty list."
update'' v (hd:tl, trail) = (v:tl, trail)

goBwd :: Zipper -> Zipper
goBwd (_, []) = error "Cannot go backwards with empty trail."
goBwd (l, Fwd x : trail) = (x : l, trail)

original :: Zipper -> [Int]
original (l, []) = l
original z = original (goBwd z)

