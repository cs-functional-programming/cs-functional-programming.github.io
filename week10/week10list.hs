data Directie = Fwd deriving (Show, Eq)

type Zipper = ([Int], [Directie])

goFwd :: Zipper -> Zipper
goFwd (list, dirs) = (list, dirs ++ [ Fwd ])

goBwd :: Zipper -> Zipper
goBwd (list, dirs) = (list, take (length dirs - 1) dirs)

getValue :: Zipper -> Int
getValue ([], _) = error "nu stiu sa iau valoarea"
getValue (hd:tl, []) = hd
getValue (hd:tl, Fwd:dirs) = getValue (tl, dirs)

initZipper :: ([Int], [Directie])
initZipper = ([1, 4, 7], [])



