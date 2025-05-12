l1 :: [Int]
l1 = [ 3, 7, 4, 6, 42 ]

-- data Crumb = Fwd Int deriving (Show, Eq, Ord)

-- type Trail = [Crumb]

--           suffix   +   prefix (reversed)
type Zipper = ([Int], [Int])

goFwd :: Zipper -> Zipper
goFwd ([], _) = error "Cannot go forward in empty list."
goFwd (hd:tl, trail) = (tl, hd : trail)

update'' :: Int -> Zipper -> Zipper
update'' _ ([], _) = error "Cannot update empty list."
update'' v (hd:tl, trail) = (v:tl, trail)

goBwd :: Zipper -> Zipper
goBwd (_, []) = error "Cannot go backwards with empty trail."
goBwd (l, x : trail) = (x : l, trail)

original :: Zipper -> [Int]
original (l, []) = l
original z = original (goBwd z)

