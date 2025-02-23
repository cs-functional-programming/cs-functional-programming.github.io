-- Transparenta referentiala (referential transparency)

-- Doua bucati de cod identice produc acelasi rezultat,
-- indiferent de contextul in care apar.

{-

Limbajul C:

int x = 3;

int g()
{
    return ++x;
}

int main()
{
  printf("%d\n", g()); // <-- expresia "g()" apare 
  printf("%d\n", g()); // in doua contexte diferite
}

Limbajul C nu se bucura de proprietatea de transparenta referentiala,
din cauza ca expresiile C pot avea efecte secundare (incrementarea
unei variabile globale, afisarea pe ecran, scrierea unui fisier,
trimiterea unui mesaj in retea, citirea unui caracter de la tastatura,
citirea pozitiei mouse-ului, ...)

-}

{-

Limbajul Haskell (pur functional):

f x = ...
            g x   -- <-- primul apel al functiei "g"
      ...
            g x   -- <-- al doilea apel al functiei "g" 
                  -- (cu acelasi argument)
      ...

Limbaje care incurajeaza codul functional, dar permit si efecte secundare:

   Ocaml, F#

Limbaje care permit scrierea de cod functional:

   C, C++, Java, C#

-}

{-

Dezavantaje cod functional:
-- nu pot face atribuiri 
-- nu am bucle for/while
-- nu pot scrie intr-un fisier (revenim la acest aspect: monada IO)...

-}

{-

Haskell un limbaj Turing-complet.

Cum efectuez calcule interesante in Haskell: functii recursive.

-}

fibo :: Integer -> Integer
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n - 1) + fibo (n - 2)

-- daca as scrie asa: fibo n = fibo n - 1 + fibo n - 2
--                                  ^ ^ ^ ^ ^^^^ ^ ^ ^
--                             (apelez fibo pe 8 argumente (give or take))
-- nu scriu asa: fibo n = fibo(n - 1) + fibo(n - 2)

{-

Model conceptual de evaluare a unei expresii:

1. se parcurge fisierul .hs de sus in jos pana la prima ecuatie care se potriveste
2. se inlocuieste partea stanga cu partea dreapta

Avantaj: modul este abstract.

-}

{-

Sintaxa de apel a unei functii in Haskell:

1. scriu numele functiei
2. scriu primul parametru
3. scriu spatiu.
4. ...
5. scriu spatiu.
6. scriu ultimul parametru

-}

-- "guard" (ro: garda)

fibo' :: Integer -> Integer
fibo' x | x == 0 || x == 1 = x
fibo' n                    = fibo' (n - 1) + fibo' (n - 2)

-- ecuatie conditionala/constransa
--            constrangerea/garda
--            vvvvvvvvvvvvvvvvvv
-- fibo' x    | x == 0 || x == 1     = x
-- ^^^^^^^                           ^ ^^^
-- patternul                           rezultatul


fibo'' :: Integer -> Integer
fibo'' x | x == 0 = 0
fibo'' x | x == 1 = 1
fibo'' n          = fibo'' (n - 1) + fibo'' (n - 2)

fibo''' :: Integer -> Integer
fibo''' x | x == 0    = 0
          | x == 1    = 1 -- zahar sintactic
          | otherwise = fibo''' (x - 1) + fibo''' (x - 2)

-- Haskell face parte din familia ML: Ocaml, Standard ML, ML/NJ, MLTON, F#, Coq, Agda, ...

fibo'''' :: Integer -> Integer
fibo'''' x = if x <= 1 then x else fibo'''' (x - 1) + fibo'''' (x - 2)

-- Prefer: pattern matching > garzi > if-then-else.

cmmdc :: Integer -> Integer -> Integer
cmmdc x 0 = x
cmmdc 0 y = y
cmmdc x y = if x > y then
              cmmdc y (x - y)
            else
              cmmdc x (y - x)

-- prefer cmmdc' in dauna cmmdc
cmmdc' :: Integer -> Integer -> Integer
cmmdc' x 0 = x
cmmdc' 0 y = y
cmmdc' x y | x > y     = cmmdc' y (x - y)
           | otherwise = cmmdc' x (y - x)

-- Haskell este un limbaj in care conteaza indentarea.

-- Numele functiilor incepem obligatoriu cu litera mica.
-- F x = x + 1
-- Literele mari sunt rezervate pentru constructori si tipuri

sumn :: Integer -> Integer
sumn 0 = 0                -- (1)
sumn n = n + sumn (n - 1) -- (2)
--           ^^^^^^^^^^^^
--           apel recursiv

-- Calcul ecuational
-- sumn 3 ==                   (2)
-- 3 + sumn 2 ==               (2)
-- 3 + (2 + sumn 1) ==         (2)
-- 3 + (2 + (1 + sumn 0)) ==   (1)
-- 3 + (2 + (1 + 0)) ==        (+)
-- 3 + (2 + 1) ==              (+)
-- 3 + 3 ==                    (+)
-- 6.                          (+)

{-
for (int i = 0; i < n; ++i) {
  s += i;
}
-}

sumna :: Integer -> Integer -> Integer
sumna 0 a = a                      -- (A)
sumna n a = sumna (n - 1) (a + n)  -- (B)
--          ^^^^^^^^^^^^^^^^^^^^^
--          apelul recursiv este in coada functiei

-- Calculul ecuational
-- sumna 3 0 ==        (B)
-- sumna 2 3 ==        (B)
-- sumna 1 5 ==        (B)
-- sumna 0 6 ==        (A)
-- 6.

-- Compilatorul transforma orice functie tail-recursive intr-o bucla.


{-

int sumna(int n, int a)
{
    if (n == 0) {
        return a;
    } else {
        return sumna(n - 1, a + n);
        //     ^^^^^^^^^^^^^^^^^^^^
        //     apelul in coada
    }
}

-}

-- sumn :: Integer -> Integer
-- sumn 0 = 0                -- (1)
-- sumn n = n + sumn (n - 1) -- (2)
-- sumna :: Integer -> Integer -> Integer
-- sumna 0 a = a                      -- (A)
-- sumna n a = sumna (n - 1) (a + n)  -- (B)

-- Rationament ecuational

-- pentru orice numar natural n: sumna n a = a + sumn n.
-- rezulta imediat (a = 0): sumna n 0 = 0 + sumn n

-- Cazul de baza (n = 0): sumna 0 a = a + sumn 0.
-- sumna 0 a
--      =             (A)
-- a
--      =             (matematica)
-- a + 0
--      =             
-- a + (sumn 0)        (1)

-- Cazul inductiv (n > 0):
-- presupun sumna (n - 1) a = a + sumn (n - 1) (II)
-- arat     sumna n a = a + sumn n

-- sumna n a
--      =                (B)
-- sumna (n - 1) (n + a)
--      =                (II)
-- (n + a) + sumn (n - 1)
--      =                (matematica)
-- a + (n + sumn (n - 1)) 
--      =                (2)
-- a + sumn n.

-- tuplu

-- pairup :: Char -> Bool -> (Char, Bool)
-- pairup x y = (x, y)

-- myfst :: (Char, Bool) -> Char
-- myfst (x, _) = x

-- mysnd :: (Char, Bool) -> Bool
-- mysnd (_, y) = y

pairup :: a -> b -> (a, b)
pairup x y = (x, y)

myfst :: (a, b) -> a
myfst (x, _) = x

mysnd :: (a, b) -> b
mysnd (_, y) = y

-- In partea stanga a ecuatiilor, nu am voie sa fac apeluri de functie
-- Exemplu negativ: f (x + y) = x <--- nu este permis
--                    ^^^^^^^
--                apel de functie in pattern (nu se poate)

-- In partea stanga a ecuatiilor, am voie sa folosesc constructori
-- Constructori = plic
-- (x, y) = un plic care contine doua elemente, x si respectiv y.



-- cmmdc :: Integer -> Integer -> Integer
-- cmmdc x 0 = x
-- cmmdc 0 y = y
-- cmmdc x y = if x > y then
--               cmmdc y (x - y)
--             else
--               cmmdc x (y - x)

-- Este o diferenta fundamentala intre cmmdc si cmmdc''
-- cmmdc are doua argumente
-- cmmdc'' are un singur argument, care este un tuplu
cmmdc'' :: (Integer, Integer) -> Integer
cmmdc'' (0, y) = y
cmmdc'' (x, 0) = x
cmmdc'' (x, y) = if x > y then
                   cmmdc'' (y, (x - y))
                 else
                   cmmdc'' (x, (y - x))

-- De obicei: prefer varianta cu mai multe argumente in dauna celei cu tuplu.

-- Exercitiu:

swap :: (a, b) -> (b, a)
-- fara sa vad implementarea functiei swap: stiu deja ce face (teoreme gratuite)
swap (x, y) = (y, x)


-- Exercitiu pt laborator: myfst (swap (pairup x y)) = y pentru orice x, y.
--                         swap (swap (pairup x y)) = pairup x y
-- folosind un rationament ecuational.

a :: Integer
a = 123


-- In Haskell, sistemul de tipuri nu poate exprima anumite lucruri (de "detaliu").
-- Exemplu:
--   -- impartirea la 0
--   -- terminarea

f :: Integer -> Integer
f x = 5 `div` x

g :: Integer -> Integer
g x = g (x - 1)

b :: Integer
b = b
