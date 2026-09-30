type Zipper = ([Int], [Int])

-- zipper-ul ( l1, l2 ) reprezinta lista    (reverse l1) ++ l2

goFwd :: Zipper -> Zipper
goFwd ([], dirs) = error "nu pot inainta in lista vida"
goFwd (hd:tl, dirs) = (tl, hd : dirs)

goBwd :: Zipper -> Zipper
goBwd (_, []) = error "nu pot sa ma intorc"
goBwd (list, x : dirs) = (x : list, dirs)

getValue :: Zipper -> Int
getValue ([], _) = error "nu stiu sa iau valoarea"
getValue (hd:tl, _) = hd

updateValue :: Int -> Zipper -> Zipper
updateValue newValue ([], _) = error "nu pot schimba valoarea"
updateValue newValue (hd:tl, dirs) = (newValue:tl, dirs)

initZipper :: ([Int], [Int])
initZipper = ([1, 4, 7], [])


{-

data Tree = Nod Int [Tree]

                      10
          4      5         6     7             
         1 2   1 2 3      13     e

-}
