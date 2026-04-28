data Directie = Fwd deriving (Show, Eq)

type Zipper = ([Int], [Directie])

goFwd :: Zipper -> Zipper
goFwd ([], dirs) = error "nu pot inainta in lista vida"
goFwd (hd:tl, dirs) = (tl, Fwd : dirs)

-- goBwd :: Zipper -> Zipper
-- goBwd (list, dirs) = (list, take (length dirs - 1) dirs)

getValue :: Zipper -> Int
getValue ([], _) = error "nu stiu sa iau valoarea"
getValue (hd:tl, _) = hd

initZipper :: ([Int], [Directie])
initZipper = ([1, 4, 7], [])

