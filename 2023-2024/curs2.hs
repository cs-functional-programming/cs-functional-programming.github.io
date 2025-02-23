




sum' :: Int -> Int
-- intoarce suma numerelor de la 0 la n (inclusiv)
sum' 0 = 0
sum' n = n + sum' (n - 1)

sum'' :: Int -> Int -> Int
-- intoarce a + suma numerelor de la 0 la n (inclusiv)
sum'' 0 a = a
sum'' n a = sum'' (n - 1) (a + n)

sum''' :: Int -> Int -> Int -> Int
-- intoarce a + suma numerelor intre i (inclusiv) si n (exclusiv)
sum''' n i a = if n == i then
                 a
               else
                 sum''' n (i + 1) (a + i)

sum'''' :: Int -> Int -> Int -> Int
sum'''' n i a | n == i = a
              | otherwise = sum'''' n (i + 1) (a + i)

dist :: Float -> Float -> Float -> Float -> Float
dist x1 y1 x2 y2 = let dx = x2 - x1 in
                   let dy = y2 - y1 in
                   sqrt (dx * dx + dy * dy)

dist' :: Float -> Float -> Float -> Float -> Float
dist' x1 y1 x2 y2 = sqrt (dx * dx + dy * dy)
  where dx = x2 - x1
        dy = y2 - y1

isprime :: Int -> Bool
isprime x = isprime_aux x 2

isprime_aux :: Int -> Int -> Bool
isprime_aux x i = if i <= x - 1 then
                     if mod x i == 0 then
                        False
                     else
                        isprime_aux x (i + 1)
                  else
                     True


isprime' :: Int -> Bool
isprime' x = nu_are_div x 2 (x - 1)

nu_are_div :: Int -> Int -> Int -> Bool
nu_are_div x a b | a > b = True
                 | mod x a == 0 = False
                 | otherwise = nu_are_div x (a + 1) b

cmmdc :: Int -> Int -> Int
cmmdc x y | y == 0 = x
          | otherwise = cmmdc y (mod x y)

cmmdc' :: (Int, Int) -> Int
cmmdc' (x, y) | y == 0 = x
              | otherwise = cmmdc' (y, mod x y)

swap :: (Int, Int) -> (Int, Int)
swap (x, y) = (y, x)

swap' :: (Int, Bool) -> (Bool, Int)
swap' (x, y) = (y, x)

swap'' :: (a, b) -> (b, a)
swap'' (x, y) = (y, x)

fifth :: (a, b, c, d, e) -> e
fifth (_, _, _, _, v) = v
