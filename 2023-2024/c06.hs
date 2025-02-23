-- Polimorfism in Haskell

length' :: [Int] -> Int
length' [] = 0
length' (hd:tl) = 1 + length' tl

length'' :: [Bool] -> Int
length'' [] = 0
length'' (hd:tl) = 1 + length'' tl

length''' :: [a] -> Int
-- "a" este o variabila de tip
-- "a" poate fi inlocuit de sistem cu orice tip (e.g., Int, Bool, [Int])
length''' [] = 0
length''' (hd:tl) = 1 + length''' tl

-- polimorfism parametric


-- polimorfismul adhoc

-- sort este polimorfica adhoc
sort :: Ord a => [a] -> [a]
sort [] = []
sort (hd:tl) = sort (filter (<=hd) tl) ++ [hd] ++ sort (filter (>hd) tl)

-- sort' este polimorfica parametric
sort' :: (a -> a -> Bool) -> [a] -> [a]
sort' c [] = []
sort' c (hd:tl) = sort' c (filter (c hd) tl) ++ [hd] ++ sort' c (filter (\x -> not (c hd x)) tl)

-- de ce as prefera polimorfismul parametric?

-- 1. mai general
-- 2. mai eficient (potential)
-- 3. mai interesant :P

f :: a -> a
f x = x

f' :: a -> a
f' x = f' x

g :: a -> Int
g x = 13

g' :: a -> Int
g' x = g' x

h :: a -> b
h x = h x
-- h x = 13

-- i :: [a] -> [a]
-- Promit     : i [1  ,  2 ,  3 ] = [  1,   3]
-- Obligatoriu: i ['a', 'b', 'c'] = ['a', 'c']
-- Promit     : i [1  ,  2 ,  1 ] = [  1,   1]
-- Q          : i ['a', 'b', 'c'] = ['a', 'a'], ['a', 'c'], ['c', 'c'], ['c', 'a']

-- Teorema (gratuita):
-- Pentru orice lista l : [a],
-- Pentru orice functie f : a -> b,
--  i (map f l) = map f (i l)

-- ∀ l : [a] . ∀ f : a -> b . i (map f l) = map f (i l).

-- Exemplu: l = [1, 2, 3]
--          f 1 = 'a', f 2 = 'b', f 3 = 'c'
--     i ['a', 'b', 'c'] = map f (i [1, 2, 3])

data Arb a = Leaf | Node a (Arb a) (Arb a) deriving Show

insert :: Ord a => Arb a -> a -> Arb a
insert Leaf x = (Node x Leaf Leaf)
insert (Node y l r) x = if x < y then
                          (Node y (insert l x) r)
                        else
                          (Node y l (insert r x))


-- Evaluare nerabdatoare (eager evaluation): C/C++/Java/C#

-- Cum se evalueaza o expresie de atribuire:
-- x = f(6) * h(7);
-- 1. Se evalueaza expresia din partea dreapta (f(6) * h(7));
-- 2. Se stocheaza rezultatul in x.

-- Evaluare lenesa (lazy evaluation): Haskell

ff :: Int -> Int
ff 0 = 1
ff 1 = 1
ff x = ff (x - 1) + ff (x - 2)

h' :: Int -> Int
h' 0 = 1
h' 1 = 1
h' x = h' (x - 1) + h' (x - 2)

x = ff 34


-- Intr-un limbaj eager: cum se evalueaza un apel de functie
-- 1. Se evalueaza fiecare argument
-- 2. Se apeleaza functia cu valorile obtinute

asdf :: Int -> Int -> Int
asdf x y = if x < 10 then
             y
           else
             x + 78

ite :: Bool -> Int -> Int -> Int
ite b x y = if b then
              x
            else
              y

double :: Int -> Int
double x = x + x

lista :: Int -> [Int]
lista x = x : (lista (x + 1))

-- prime :: [Int]
