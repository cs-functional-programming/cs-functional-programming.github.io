-- transparenta referentiala

-- "daca apare de doua ori in cod aceeasi expresie,
-- sigur va avea acelasi rezultat"

g :: Int -> Int
g x = x + 8 -- secret

use_g :: Int -> Int -> Int
use_g a b = (g 7) + (g 7)
--          ^^^^^   ^^^^^
--        prima ap   a doua ap
--   garantie: cele doua aparitii sigur
--   se evalueaza la aceeasi valoare
--   datorita proprietatii de transparenta
--   referentiala
--   nu este nicio sansa ca prima ap.
--   a lui "g 7" sa intoarca 10
--   si a doua sa intoarca 11.


-- Efecte secundare
-- Side effects
-- 1. incrementarea unei variabile
-- 2. actualizarea valorii unei variabile
--    (atribuirea)
-- 3. afisarea unui text la iesirea std
-- 4. stergerea unui fisier
-- 5. scrierea intr-un fisier
-- 6. trimiterea unui mesaj pe retea

-- Expresiile din limbajele
-- pur functionale nu au
-- efecte secundare.

a :: Int
a = 10

b :: Int
b = b + 1

y :: Int
y = 0

g' :: Int -> Int
g' x = let y = y + 1 in
        -- ^^^^^^^^^
        -- nu este o incrementare
        -- ci o definire a lui y
        -- ca fiind y + 1
        -- In Haskell: nu pot schimba
        -- valoarea unei variabile
        x + y

use_g' :: Int -> Int -> Int
use_g' a b = (g' 7) + (g' 7)

g'' :: Int -> Int
g'' x = let y = 12 in -- am declarat alta
        x + y         -- variabila "y"
                      -- cu domeniul de
                      -- vizibilitate mai
                      -- mic 

use_g'' :: Int -> Int -> Int
use_g'' a b = (g'' 7) + (g'' 7)

{-
int s = 0;
int (int i = 0; i < n; ++i) {
  s += i;
}
-}
s :: Int
s = 0

-- functie_suma :: Int -> Int
-- functie_suma n = let i = 0 in (???)
--                    s = s + i
--                    i = i + 1

mysum :: Integer -> Integer
-- pp. ca argumentul este >= 0
mysum 0 = 0
mysum n = n + (mysum (n - 1))

-- functia foloseste O(n) stiva
-- si "dureaza" 2 x n unitati de timp,
-- adica de 2 ori mai mult timp decat
-- un for echivalent

-- aceste functii pot fi rescrise
-- astfel incat sa evite consumul suplimentar
-- de memorie si timpul suplimentar de lucru

-- secretul: folosirea unui parametru suplimentar
-- care se numeste "acumulator"

-- apelul mysum_aux n a calculeaza a + 1 + 2 + 3 + ... + n
mysum_aux :: Integer -> Integer -> Integer
mysum_aux 0 a = a
mysum_aux n a = mysum_aux (n - 1) (a + n) -- tail call
                                          -- in "pozitie de coada"
                -- tail recursive
                -- recursivitate "in coada"
-- Haskell: garantie ca orice functie tail recursive foloseste O(1) stiva


-- mysum :: Integer -> Integer
-- mysum 0 = 0
-- mysum n = n + (mysum (n - 1))

mysum' :: Integer -> Integer
mysum' n = mysum_aux n 0

{-
mysum' 5 = 
mysum_aux 5 0 =
mysum_aux 4 (0 + 5) =
mysum_aux 3 (0 + 5 + 4) =
mysum_aux 2 (0 + 5 + 3 + 2) =
mysum_aux 1 (0 + 5 + 3 + 2 + 1) =
mysum_aux 0 (0 + 5 + 3 + 2 + 1 + 0) =
0 + 5 + 3 + 2 + 1 + 0 =
-}


-- Suntem siguri ca mysum n si mysum' n calculeaza aceeasi valoare?
-- Rationament ecuational ca sa fac demonstratii interesante.

-- Fie n numar natural oarecare.
-- Arat a + mysum n == mysum_aux n a prin inductie.
-- Cazul I (baza). n == 0
--      a + mysum n
--        == -- prin definitie
--      a + 0
--        ==
--      a
--        ==
--      mysum_aux 0 a
--        ==
--      mysum_aux n a
-- Cazul II (inductiv). n != 0
--      a + mysum n
--        == -- prin definitie
--      a + (n + (mysum (n - 1)))
--        ==
--      (a + n) + mysum (n - 1)
--      ^^^^^^^         ^^^^^^ 
--        == -- ipoteza de inductie
--      mysum_aux (n - 1) (a + n)
--        == -- prin definitie
--      mysum_aux n a


fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n - 1) + fibo (n - 2)

fibo_aux :: Int -> Int -> Int -> Int
fibo_aux 0 a b = a
fibo_aux 1 a b = b
fibo_aux n a b = fibo_aux (n - 1) b (a + b)

fibo' :: Int -> Int
fibo' n = fibo_aux n 0 1

-- invariant: a = fibo (n - 1), b = fibo n



fibo'' :: Int -> Int
fibo'' n = fibo_aux n 4 7


-- fibo_aux 10 4 7 === generalizare a sirului lui Fibonacci
-- Concrete Mathematics (D Knuth)
