-- Polimorfism ad-hoc vs parametric

data Nat = Zero | Succ Nat deriving (Show, Eq)

-- Nat instanta a claselor Show, Eq

instance Ord Nat where
  (<=) Zero _ = True
  (<=) (Succ n) Zero = False
  (<=) (Succ n) (Succ m) = n <= m

-- constrangere de tip
--    vvvvv
qs :: Ord a => [a] -> [a] -- polimorfism ad-hoc
qs [] = []
qs (hd:tl) = qs (filter (<=hd) tl) ++ [ hd ] ++ qs (filter (>hd) tl)


-- Polimorfism in Haskell
-- (a) parametric: variabilele de tip "a" care nu sunt constranse
-- (b) ad-hoc: variabilele de tip "a" care apar intr-o constrangere de clasa

hd :: [a] -> Maybe a -- polimorfism parametric
hd [] = Nothing
hd (x:_) = Just x

-- Avantaj 1 pentru polimorfism parametric: e mai general

instance Show (Integer -> Integer) where
  show f = "<functie>"


ex1 :: a -> b
-- ex1 x = ex1 x -- (Varianta 1)
ex1 _ = error "exceptie in timpul evaluarii" -- (Varianta 2)
-- Daca evaluarea expresiei "ex1 'a'" produce o exceptie, atunci
-- 100% si evaluarea expresiei "ex1 17" tot o exceptie produce.

-- Avantaj 2: signatura ofera deja mai multa informatie


ex2 :: a -> a
-- Varianta 1: functia identitate
-- Varianta 2: merge la infinit pentru orice input
-- Varianta 3: exceptie pentru orice input
ex2 x = x


count :: [a] -> Int
count [] = 0
count (hd : tl) = 1 + count tl

ex3 :: [a] -> a
-- obligatoriu, daca intoarce ceva (nu merge la infinit sau arunca o exceptie),
-- atunci intoarce unul dintre elementele listei
-- Pentru lista vida, obligatoriu: (1) exceptie / (2) merg la infinit
ex3 l@(hd1 : hd2 : hd3 : tl) | mod (count l) 2 == 0 = hd2
ex3 l@(hd1 : hd2 : hd3 : tl) | otherwise            = hd3

-- Garantie: daca ex3 [ i_1, i_2, i_3, i_4 ] = i_k (unde i_i :: Int),
-- atunci sigur ex3 [ c_1, c_2, c_3, c_4 ] = c_k (unde c_i :: Char).

-- Cand folosesc polimorfism ad-hoc (constrangeri de tipuri)
--   - cand nu se poate altfel.
-- De exemplu, la functia de sortare.
-- Daca vreau sa testez egalitatea/dizegalitatea a doua elemente.

-- nu exista o implementare pentru functia
-- egal :: a -> a -> Bool

-- as obtine si o implementare pentru
-- egalitatea de functii:
-- egal :: (Int -> Int) -> (Int -> Int) -> Bool (nedecidabila)


-- Un dezavantaj al polimorfismului ad-hoc:

-- Pentru functia
--   qs :: Ord a -> [a] -> [a]
-- compilatorul adauga un parametru suplimentar
--   qs :: Tabel -> [a] -> [a]
-- unde Tabel contine o lista de pointeri la functiile care implementeaza
-- clasa Ord.
-- qs ['a', 'c', 'b'] ---> qs Tabel_Char ['a', 'c', 'b']
-- qs [5 :: Int, 3, 2] --> qs Tabel_Int [5, 3, 2]
--        5 <= 3 ---> Tabel_Int["<="] 5 3

-- Dezavantaj polimorfism ad-hoc: un tip de date nu poate fi instanta
-- a unei clase decat intr-un singur fel.

-- instance Ord String

-- "asdf100" > "asdf99" -- as avea nevoie sa imi fac inca o instanta
-- a Ord String pentru sortare in ordinea naturala.

myf :: Int -> Int -> Bool
myf x y = x <= y

-- Solutie:
qs' :: (a -> a -> Bool) -> [a] -> [a]
qs' _ [] = []
qs' f (hd:tl) = qs' f (filter (\x -> f x hd) tl) ++ [hd] ++
                qs' f (filter (\x -> not (f x hd)) tl)


comp_str :: String -> String -> Bool
comp_str x y = ((read x) :: Integer) <= ((read y) :: Integer)

-- Evaluare leneșă


{-

MARCA A UNEI DE STRATEGII DE EVALUARE "EAGER" (nerabdatoare)

Cand se executa o instructiune de atribure:

   x = expresie;

se evalueaza expresia "expresie", iar rezultatul se stocheaza in
variabila x.

Cand se apeleaza functia f cu parametrii expr_1, expr_2, ..., expr_n

  f(expr_1, expr_2, ..., expr_n)

se evalueaza valorile expresiilor expr_i si apoi se apeleaza f
cu valorile respective.

-}

-- lazy evaluation

-- fib x = phi ^ x          (phi = 1.61)
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib (x - 2) + fib (x - 1) -- (*)

f :: Int -> Int
f x = 2 -- (**)

a :: Int
a = fib 100

b :: Int -> Int
b x = let y = fib 100 in -- (***)
      let z = x + 10 in
        if z > 20 then
          y
        else
          13

-- "a" contine o promisiune
-- "daca cineva are nevoie de un Int, pot eu sa il produc"
-- "a" e un pointer la un "thunk" (bucata de cod)
-- cand rulez "thunk"-ul, apare un Int

-- daca Haskell ar fi avut o evaluare "eager":
-- f (fib 100)
--   == (*)
-- f (fib 98 + fib 99)
--   == (*)
-- f ((fib 96 + fib 97) + fib 99)
--   == (*) ...

-- dar Haskell o strategie de evaluare "lazy":
-- nu evaluez o expresie decat daca sunt absolut fortat sa fac acest lucru

-- f (fib 100)
--   == (**)
-- 2

-- if True  then e1 else e2 = e1 (IF1)
-- if False then e1 else e2 = e2 (IF2)
{-
 b 3
   == (***)
 let y = fib 100 in
   let z = 3 + 10 in
     if z > 20 then
       y
     else
      13
   == (limbaj)
 let z = 3 + 10 in
   if z > 20 then
     fib 100
   else
    13
 == (limbaj)
   if 3 + 10 > 20 then
     fib 100
   else
    13
 == (limbaj)
    13
-}

ite :: Bool -> a -> a -> a
ite True e1 e2 = e1
ite False e1 e2 = e2

b' :: Int -> Int
b' x = let y = fib 100 in -- (***)
       let z = x + 10 in
         ite (z > 20) y 13

double :: Int -> Int
double x = x + x -- (*)

patrat :: Int -> Int
patrat x = x * x -- (**)

-- patrat (double 10)
--   == (**)
-- (double 10) * (double 10)
--   == (*)
-- (10 + 10) * (10 + 10)
--   == (arithmetic)
-- 400

-- patrat (fib 33)
--   == (**)
-- (fib 33) * (fib 33)
--   == 
-- 3524578 * 3524578
--   == (arithmetic)
-- <big_number>

-- Exista mai multe strategii de evaluare lazy
-- Haskell foloseste "call-by-need"
-- Strategii de evaluare in Sapt 14 (lambda calcul)

listaAux :: Integer -> [Integer]
listaAux i = i : listaAux (i + 1)

listaNat :: [Integer]
listaNat = listaAux 0

listaPare :: [Integer]
listaPare = map (*2) listaNat

-- map :: (a -> b) -> [a] -> [b]
-- map _ [] = []
-- map f (hd:tl) = f hd : map f tl

-- (!!) :: [a] -> Int -> a
-- (!!) _ [] = error "asdf"
-- (!!) 0 (hd:tl) = hd
-- (!!) n (hd:tl) = (!!) (n-1) tl

-- (map (*2) listaNat) !! 0
--   ==
-- (map (*2) (listaAux 0)) !! 0
--   ==
-- (map (*2) (0 : listaAux 1)) !! 0
--   ==
-- ((*2) 0 : map (*2) (listaAux 1)) !! 0
--   ==
-- (*2) 0
--   ==
-- 0
