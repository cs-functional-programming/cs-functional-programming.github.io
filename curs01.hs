-- cod interesant
f :: Int -> Int
f x = x * 3

{-
int f(int x)
{
  return x * 3;
}
-}




g x = x + 4

h x = x * 17 + 13

qs [] = []
qs (h:t) = (qs (filter (<=h) t)) ++ [h] ++ (qs (filter (>h) t))


adunare x y = x + y

{- suma primelor n numere naturale strict pozitive -}

suman 0 = 0
suman n = n + (suman (n - 1))

-- suman 10 = 10 + (suman 9)
--          = 10 + (9 + (suman 8))
--          = ...

suman' n = n + (suman' (n - 1))
-- suman' 0 = 0


-- Err: asdf x = x + 3 + "asdf"



-- Haskell e "pur functional"
-- 1. schimbarea valorii unei variabile
-- 2. scrierea pe ecran
-- 3. stergerea unui fisier
-- 4. trimiterea unui mesaj in retea


{-
for (int i = 0; i < 10; ++i) {
  x += i;
}
-}


-- transparenta referentiala














