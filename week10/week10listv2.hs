data Crumb = Fwd Int deriving (Show, Eq)

type Zipper = ([Int], [Crumb])

goFwd :: Zipper -> Zipper
goFwd ([], dirs) = error "nu pot inainta in lista vida"
goFwd (hd:tl, dirs) = (tl, Fwd hd : dirs)

goBwd :: Zipper -> Zipper
goBwd (_, []) = error "nu pot sa ma intorc"
goBwd (list, Fwd x : dirs) = (x : list, dirs)

getValue :: Zipper -> Int
getValue ([], _) = error "nu stiu sa iau valoarea"
getValue (hd:tl, _) = hd

updateValue :: Int -> Zipper -> Zipper
updateValue newValue ([], _) = error "nu pot schimba valoarea"
updateValue newValue (hd:tl, dirs) = (newValue:tl, dirs)

initZipper :: ([Int], [Crumb])
initZipper = ([1, 4, 7], [])

