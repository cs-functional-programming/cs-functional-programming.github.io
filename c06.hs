-- Polimorfism
-- 1. parametric
-- 2. ad-hoc

-- Polimorfism parametric
-- valorile tipului parametru sunt tratate identic indiferent de tipul concret

-- Exemplu:

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- Polimorfismul ad-hoc
-- daca apare o restrictie de tip ==> polimorfism ad-hoc
-- comportamentul functiei este adaptat in functie de tipul care instantiaza variabila de tip

ordoneaza :: Ord a => (a, a) -> (a, a)
ordoneaza (x, y) = if x < y then -- functia "<" vine din implementarea clasei Ord de catre
                     (x, y)      -- tipul a
                   else
                     (y, x)

data Nat = Zero | Succ Nat deriving (Show, Eq)

instance Ord Nat where
  (<=) Zero _ = False
  (<=) (Succ _) Zero = True
  (<=) (Succ x) (Succ y) = x <= y

n2 = Succ (Succ Zero)
n3 = Succ (Succ (Succ Zero))

-- Sunt cateva functii imposibil de scris:

-- NU: sort :: [a] -> [a]          (polimorfism parametric)
-- DA: sort :: Ord a => [a] -> [a] (polimorfism ad-hoc)

mister :: a -> a
-- v1: mister x = x
-- v2:
mister x = mister x

mister' :: a -> b
mister' x = mister' x

-- Avantaje polimorfism ad-hoc: pot face lucruri mai "interesante"
-- Avantaje polimorfism parametric: imi dau seama ce face functia din signatura

-- Combinatii:

exf :: Num a => (a, b) -> (b, a)
exf (x, y) = (y, signum x)

-- Cum recunoastem p.p.: lipsesc constragerile de tip
--                 p.ad-hoc.: sunt prezente constrangerile de tip


-- exf2 :: [a] -> a
-- daca exf2 nu merge la infinit, atunci valoarea returnata
--                                sigur sigur va fi o valoare din lista initiala

-- TEOREME "GRATUITE"
-- daca exf2 [1, 2, 3] = 2, atunci exf2 ['A', 'B', 'C'] = 'B'
-- Philip Wadler: Theorems for Free

-- In Haskell, nu avem polimorfism ad-hoc in tipurile de date

-- Asa DA:
data ABC a = Leaf | Node a (ABC a) (ABC a) deriving (Show, Eq)
-- Asa NU:
-- data Ord a => ABC a = Leaf | Node a (ABC a) (ABC a) deriving (Show, Eq)

-- slowfind :: Eq a => a -> ABC -> Bool
-- fastfind :: Ord a => a -> ABC -> Bool

insert :: Ord a => a -> ABC a -> ABC a
insert x Leaf = Node x Leaf Leaf
insert x (Node y l r) = if x < y then
                          Node y (insert x l) r
                        else if x > y then
                          Node y l (insert x r)
                        else
                          Node y l r

-- insert 2 (insert 5 (insert 1 Leaf))
--          1
--             \
--                 5
--                /
--               2

-- II. Evaluarea Lenesa

-- 1. stricta
-- 2. lenesa

-- Instructiunea "x = exp" se executa in felul urmator:
--    1. se evalueaza expresia "exp";
--    2. valoarea expresiei se salveaza in variabila "x".

-- Apelul unei functii "f(e1, ..., en)" se evalueaza in felul urmator:
--    1. se evalueaza expresiile e1, e2, ..., en, in aceasta ordine;
--    2. valorile expresiilor inlocuiesc argumentele functiei f si se evalueaza corpul functiei f.

f :: Int -> Int -> Int
f x y = if x > 2 then
          y
        else
          x

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = (fib (x - 1)) + (fib (x - 2))

-- f 2 (fib 34) merge instant
-- f (fib 34) 2 merge incet


-- Evaluare "lenesa"
-- Lazy evaluation
-- Nicio expresie Haskell nu este evaluata decat daca este strict necesara pentru obtinerea rezultatului

-- Remember: rationamente ecuationale


-- if True then x else y = x
-- if False then x else y = y

{-
 f 2 (fib 34) =               (UNFOLD f)
 if 2 > 2 then
        fib 34
      else
        2     =               (2 > 2)
 if False then
        fib 34
       else
        2      =              (ITE)
 2
-}

ite :: Bool -> a -> a -> a
ite True x y = x
ite False x y = y

f' :: Int -> Int -> Int
f' x y = ite (x > 2) y x

-- ce e mai eficient?
-- nu stim cine este functia f.
-- f 2 (fib 34) evaluat lenes sau evaluat strict?

double x = x + x

-- 1. Evaluare stricta (double (double 2))
-- 2. Evaluare lenesa (double (double 2))

{-
1. double (double 2) = double (2 + 2) = double 4 = 4 + 4 = 8.
           ^^^^^^^^
  --> am evaluat o singura data argumentul (double 2)

2. double (double 2) = double 2 + double 2 = (2 + 2) + double 2 = 4 + double 2 = 4 + (2 + 2) = 4 + 4 = 8.
   ^^^^^^ 

   --> am evaluat de doua ori (double 2)

-}


-- Haskell foloseste o strategie call-by-need care evalueaza o expresie cel mult o data


g :: Int -> [Int]
g i = i : (g (i + 1))

listamea :: [Int]
listamea = g 5

-- lista mea este lista 5, 6, 7, 8, ...

listamea' = map (*3) listamea

-- avantaje: poate fi mai eficient
--           pot sa imi definesc structuri infinite
-- dezavantaje: este dificil de estimat timpul de executie al unei functii


-- In Haskell, o variabila - nu tine minte o valoare
--                         - tine un pointer spre un program (= thunk)
--                           pe care, daca il execut la un moment dat in viitor,
--                           produce valoarea de care am nevoie


-- dezavantaj al evaluarii pure: -- nu pot face structuri circulare
-- evaluarea lenesa (to some extent) reduce acest dezavantaj
lista = 4 : 5 : 6 : lista

-- joc de X si 0

data Cell = Z | X | E deriving (Eq, Show)

-- "type" introduce un sinonim de tip
type Line = (Cell, Cell, Cell)

type Board = (Line, Line, Line)

data Player = PZ | PX deriving (Eq, Show)

succCell :: Cell -> Player -> [Cell]
succCell E PX = [ X ]
succCell E PZ = [ Z ]
succCell _ _ = []

succLine :: Line -> Player -> [Line]
succLine (c1, c2, c3) p = (map (\x -> (x, c2, c3)) (succCell c1 p)) ++
                          (map (\x -> (c1, x, c3)) (succCell c2 p)) ++
                          (map (\x -> (c1, c2, x)) (succCell c3 p))
-- TODO: de revenit si rezolvat problema cu DRY

emptyLine = (E, E, E)

empty :: Board
empty = (emptyLine, emptyLine, emptyLine)

succBoard :: Board -> Player ->  [Board]
succBoard (l1, l2, l3) p = (map (\x -> (x, l2, l3)) (succLine l1 p)) ++
                            (map (\x -> (l1, x, l3)) (succLine l2 p)) ++
                            (map (\x -> (l1, l2, x)) (succLine l3 p))
-- TODO: de revenit si rezolvat problema cu DRY
                          
type Config = (Board, Player)

configInit = (empty, PX)

other :: Player -> Player
other PX = PZ
other PZ = PX

succConfig :: Config -> [Config]
succConfig (board, p) = map (\x -> (x, other p)) (succBoard board p)

data Arb = Nod Config [Arb] deriving (Show, Eq)

terminal :: Config -> Bool
terminal x = length (succConfig x) == 0

arbAux :: Config -> Arb
arbAux config = Nod config (map arbAux (succConfig config))

arb :: Arb -- <-- imposibil (cel putin dificil) de definit intr-un limbaj eager
arb = arbAux configInit

get :: Arb -> [Int] -> Config
get (Nod x _) [] = x
get (Nod x s) (hd:tl) = get (s !! hd) tl
