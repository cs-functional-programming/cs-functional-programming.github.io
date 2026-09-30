-- (1) Polimorfismul din Haskell
-- (2) Evaluare leneșă

-- (A) Polimorfism parametric
data List a = Empty | Cons a (List a) deriving (Eq, Show)

-- polimorfism parametric
len :: List a -> Int
len Empty = 0
len (Cons hd tl) = 1 + len tl

-- (B) Polimorfism ad-hoc
qs :: Ord a => [a] -> [a]
qs [] = []
qs (hd:tl) = qs (filter (<=hd) tl) ++ [ hd ] ++ qs (filter (>hd) tl)


-- Tipurile de date in Haskell suporta doar polimorfism parametric.

-- nu se poate face:
-- data Ord a => ABC a = Leaf | Node a ABC ABC deriving (Eq, Show)

-- ce se face in schimb
data ABC a = Leaf | Node a (ABC a) (ABC a) deriving (Eq, Show)

-- search :: Ord a => a -> ABC a -> Bool
--           ^^^^^

f :: Ord a => a -> a -> b -> b -> b
f x y v1 v2 = if x < y then v1 else v2

mistery :: a -> a
mistery x = x

mistery' :: a -> a
mistery' x = mistery' x

mistery'' :: a -> a
mistery'' x = undefined

mistery''' :: a -> a
mistery''' x = error "haha"

mistery1 :: a -> b
mistery1 x = error "asdf"

mistery1' :: a -> b
mistery1' x = mistery1' x


mistery2 :: [a] -> a
mistery2 (hd:tl) = hd

-- mistery2' [ 1, 4, 42 ] = 1
-- mistery2' [ 1, 4, 7, 42 ] = 4

mistery2' :: [a] -> a
mistery2' (hd1:hd2:hd3:[]) = hd1
mistery2' (hd1:hd2:hd3:hd4:[]) = hd2

-- mistery3 [ 1, 4, 42 ] = 1
-- mistery3 [ "abc", "def", "ghi" ] = "def"

--mistery3 :: [a] -> a

-- Teoreme gratuite:
--    forall f :: [a] -> a, f [1, 2, 3] == 2 ==> f ["a", "b", "c"] = "b"


-- mistery4 :: Ord a => a -> a -> a
-- mistery4 x y = if x <= y then x else y

-- mistery4 :: Eq a => a -> a -> a
-- mistery4 x y = if x == y then x else y

-- Evaluarea lenesa

{-

Eager evaluation
"Nu lasa pe maine ce poti face azi"

x = e; // se evalueaza expresia e, iar rezultatul este pus in variabila x

f(e1, e2, ..., en) // se evalueaza e1, e2, ..., en, rezultatele expresiilor se pun pe stiva si apoi se apeleaza f

-}

{-

Lazy evaluation
"Ce poti face azi lasa pe maine, poate nu mai trebuie"

lazy evaluation = nicio expresie nu este evaluata decat in momentul in care este
                  strict necesara valoarea ei

-}
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib (x - 2) + fib (x - 1)

fib_aux :: Int -> Int -> Int -> Int
fib_aux a b 0 = a
fib_aux a b n = fib_aux b (a + b) (n - 1)

fib' :: Int -> Int
fib' n = fib_aux 0 1 n

a :: Int
a = let x = fib 100 in 2 * 2

g :: Int -> Int -> Int -> Int
g x y z = if x < 10 then
            y
          else
            z

-- Sa ne imaginam cum s-ar evalua expresia 
-- g 16 (fib' 100) 7
-- daca Haskell ar suporta eager evaluation

-- g 16 (fib 100) 7 ==
-- ...                  (mai multi pasi: c^100 pasi, c = 1.61...)
-- g 16 3736710778780434371 7 ==
-- if 16< 10 then
--          3736710778780434371
--        else
--          7 ==
-- if False then
--          3736710778780434371
--        else
--          7 ==
-- 7.

-- Lazy evaluation
-- g 16 (fib 100) 7 ==
-- if 16 < 10 then
--          fib 100
--        else
--          7 ==
-- if False then
--          fib 100
--        else
--          7 ==
-- 7.

ite :: Bool -> a -> a -> a
ite True x y = x
ite False x y = y

-- Implementare: fiecare variabila in Haskell tine minte nu o valoare,
--               ci este un pointer spre o bucata de cod
--                                         ^^^^^^^^^^^^^
--                                             thunk
--               bucata de cod, daca este executata, produce valoarea variabilei

-- Subtilitate:
double :: Int -> Int
double x = x + x
-- exista mai multe strategii de evaluare lazy (call-by-name)
-- Haskell foloseste o strategie care se numeste call-by-need
-- double (fib 31) =
-- (fib 31) + (fib 31) =
-- ^^^^^^^^   ^^^^^^^^
--   1.12s      1.12s
-- 1346269 + 1346269 = 
-- 26...

-- Relatia cu scurtcircuitarea

listaAux :: Int -> [Int]
listaAux n = n : listaAux (n + 1)

listaNat :: [Int] -- este o lista (infinita) cu toate numerele naturale
listaNat = listaAux 0

-- Dezavantaj al evaluarii lazy: dificil de estimat complexitatea-timp a unui algoritm

-- O utilitate importanta pentru evaluarea lazy: permite scrierea de programe imperative
--                                               intr-un stil pur functional
