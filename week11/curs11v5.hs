l1 :: [Int]
l1 = [ 3, 7, 4, 6, 42 ]

type Zipper = ([Int], [Int])

goFwd :: Zipper -> Maybe Zipper
goFwd ([], _) = Nothing
goFwd (hd:tl, trail) = Just (tl, hd : trail)

update'' :: Int -> Zipper -> Maybe Zipper
update'' _ ([], _) = Nothing
update'' v (hd:tl, trail) = Just (v:tl, trail)

goBwd :: Zipper -> Maybe Zipper
goBwd (_, []) = Nothing 
goBwd (l, x : trail) = Just (x : l, trail)

-- original :: Zipper -> [Int]
-- original (l, []) = l
-- original z = original (goBwd z)

listOf :: Zipper -> [Int]
listOf (l, _) = l

original :: Zipper -> [Int]
original z = case goBwd z of
               Nothing -> listOf z
               Just z' -> original z'

followDirs :: Zipper -> Maybe Zipper
followDirs z0 = do z1 <- goFwd z0
                   z2 <- goFwd z1
                   z3 <- goFwd z2
                   z3' <- update'' 13 z3
                   z4 <- goBwd z3'
                   return z4
                