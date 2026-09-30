-- Clase de tipuri

-- multimi de tipuri
-- vvvvvvvvvvvvvvvvv
-- Clasele de tipuri (in Haskell) != clasele in C++
--                                   ^^^^^^^^^^^^^^
--                                     tipuri de date

data Dow = Mon | Tue | Wed

-- GHCI este un REPL (read-evaluate-print loop)

-- ghci> Mon
-- Mon

-- <interactive>:3:1: error:
--     • No instance for (Show Dow) arising from a use of ‘print’
--     • In a stmt of an interactive GHCi command: print it

-- EROAREA: Tipul "Dow" nu este o instanta a clasei "Show"
--                "Dow" nu face parte din clasa de tipuri "Show"

data Dow' = Mon' | Tue' | Wed' deriving Show

-- Show: clasa acelori tipuri de date care pot fi transformate in String

-- show: functia
-- show :: Show a => a -> String
--         ^^^^^^
--       constrangere


data Dow'' = Mon'' | Tue'' | Wed''

instance Show Dow'' where
  --show :: Dow'' -> String
  show Mon'' = "Luni"
  show Tue'' = "Marti"
  show Wed'' = "Miercuri"

-- Un de tip de date poate face parte dintr-o clasa intr-un singur mod
-- instance Show Dow'' where
--   --show :: Dow'' -> String
--   show Mon'' = "Mon"
--   show Tue'' = "Tue"
--   show Wed'' = "Wed"


-- Show = clasa acelor tipuri de date care pot fi transformate in String-uri

instance Show (Integer -> Integer) where
  show f = "<functie>"

-- Ce alte clase de tipuri mai sunt definite in sistem


-- Clasa Eq

-- ghci> Mon'' == Tue''
-- Mon'' == Tue''

-- <interactive>:26:7: error:
--     • No instance for (Eq Dow'') arising from a use of ‘==’
--     • In the expression: Mon'' == Tue''
--       In an equation for ‘it’: it = Mon'' == Tue''

data Dow''' = Mon''' | Tue''' | Wed''' deriving (Show, Eq)

data Dow4 = Mon4 | Tue4 | Wed4 deriving Show

instance Eq Dow4 where
  (==) Mon4 Mon4 = True
  (==) Tue4 Tue4 = True
  (==) Wed4 Wed4 = True
  (==) _ _ = False
  -- (/=) x y = not (x == y)        -- Daca vreau: implementare explicita a lui (/=)

data Dow5 = Mon5 | Tue5 | Wed5 deriving Show

instance Eq Dow5 where
  (/=) Mon5 Mon5 = False
  (/=) Tue5 Tue5 = False
  (/=) Wed5 Wed5 = False
  (/=) _ _ = True

-- datoria programatorului sa se asigure ca instanta este definita in mod consistent
data Dow6 = Mon6 | Tue6 | Wed6 deriving Show

instance Eq Dow6 where
  (==) x y = True
  (/=) Mon6 Tue6 = False
  (/=) x y = True
  
  
-- De ce as avea nevoie sa fac instante manual, daca sistemul poate produce instante automat folosind "deriving"?

data Nat = Zero | Succ Nat deriving (Show, Eq)

-- Zero reprezinta numarul "0"
-- Succ Zero reprezinta numarul "1"
-- Succ (Succ Zero) reprezinta numarul "2"
-- Succ (Succ (Succ Zero)) reprezinta numarul "3"

data Z = Plus Nat | Minus Nat deriving (Show, Eq)

-- 3 = +3 = Plus (Succ (Succ (Succ Zero)))
-- -2 = Minus (Succ (Succ Zero))

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- ghci> Plus Zero == Minus Zero
-- Plus Zero == Minus Zero
-- False

data Z' = Plus' Nat | Minus' Nat deriving Show

instance Eq Z' where
  (==) (Plus' x) (Plus' y) = x == y
  (==) (Minus' x) (Minus' y) = x == y
  (==) (Plus' Zero) (Minus' Zero) = True
  (==) (Plus' _) (Minus' _) = False
  (==) (Minus' Zero) (Plus' Zero) = True      -- de datoria mea sa ma asigur ca egalitatea este simetrica
  (==) (Minus' _) (Plus' _) = False
  
-- Ce alte clase de tipuri mai exista?

-- Clasa Ord
-- class Eq a => Ord a where
--   compare :: a -> a -> Ordering
--   (<) :: a -> a -> Bool
--   (<=) :: a -> a -> Bool
--   (>) :: a -> a -> Bool
--   (>=) :: a -> a -> Bool
--   max :: a -> a -> a
--   min :: a -> a -> a
--   {-# MINIMAL compare | (<=) #-}
  

data Nat' = Zero' | Succ' Nat' deriving (Show, Eq, Ord)

data Nat'' = Succ'' Nat'' | Zero'' deriving (Show, Eq, Ord)

-- EROARE:
--- data Nat''' = Succ''' Nat | Zero''' deriving (Show, Eq, Ord)

data Nat4 = Succ4 Nat4 | Zero4 deriving (Show, Eq)

instance Ord Nat4 where
  (<=) Zero4 _ = True
  (<=) _ Zero4 = False
  (<=) (Succ4 x) (Succ4 y) = x <= y -- apel recursiv "4 <= 5" = "3 <= 4"
  
-- Exercitiu: instance Ord Z' where ...

-- Clasa Bounded

data Dow7 = Mon7 | Tue7 | Wed7 deriving (Bounded, Show)

-- EROARE:
-- data Nat5 = Zero5 | Succ5 Nat5 deriving (Show, Eq, Bounded)

-- Clasa Enum

data Dow8 = Mon8 | Tue8 | Wed8 deriving (Bounded, Show, Enum)

-- Clasa Num
-- Clasa Integral

-- Pot sa definesc si eu propriile clase de tipuri

class MyEq a where
  egal :: a -> a -> Bool
  neegal :: a -> a -> Bool

-- Eroare
-- data Nat5 = Succ5 Nat5 | Zero5 deriving (Show, MyEq)
data Nat5 = Succ5 Nat5 | Zero5 deriving Show

-- Alternativa 1:
-- instance MyEq Nat5 where
--   egal Zero5 Zero5 = True
--   egal (Succ5 x) (Succ5 y) = egal x y
--   egal _ _ = False
--   neegal Zero5 Zero5 = False
--   neegal (Succ5 x) (Succ5 y) = neegal x y
--   neegal _ _ = True

-- Alternativa 2:
instance MyEq Nat5 where
  egal Zero5 Zero5 = True
  egal (Succ5 x) (Succ5 y) = egal x y
  egal _ _ = False
  neegal x y = not (egal x y)
  -- neegal Zero5 Zero5 = False
  -- neegal (Succ5 x) (Succ5 y) = neegal x y
  -- neegal _ _ = True


class MyEq' a where
  egal' :: a -> a -> Bool
  neegal' :: a -> a -> Bool
  neegal' x y = not (egal' x y)

instance MyEq' Nat5 where
  egal' Zero5 Zero5 = True
  egal' (Succ5 x) (Succ5 y) = egal' x y
  egal' _ _ = False
  -- neegal' x y = not (egal' x y) -- Nu mai este necesar
  
class MyEq'' a where
  egal'' :: a -> a -> Bool
  egal'' x y = not (neegal'' x y)
  neegal'' :: a -> a -> Bool
  neegal'' x y = not (egal'' x y)

instance MyEq'' Nat5 where
  neegal'' Zero5 Zero5 = False
  neegal'' (Succ5 x) (Succ5 y) = neegal'' x y
  neegal'' _ _ = True
  -- egal'' Zero5 Zero5 = True
  -- egal'' (Succ5 x) (Succ5 y) = egal'' x y
  -- egal'' _ _ = False
  -- neegal' x y = not (egal' x y) -- Nu mai este necesar
  

-- Atentie: bucla infinita
class MyEq''' a where
  egal''' :: a -> a -> Bool
  egal''' x y = not (neegal''' x y)
  neegal''' :: a -> a -> Bool
  neegal''' x y = not (egal''' x y)

instance MyEq''' Nat5 where

class MyEq'''' a where
  egal'''' :: a -> a -> Bool
  egal'''' x y = not (neegal'''' x y)
  neegal'''' :: a -> a -> Bool
  neegal'''' x y = not (egal'''' x y)
  {-# MINIMAL egal'''' | neegal'''' #-}

-- Warning:
-- instance MyEq'''' Nat5 where

-- 
data ABC a = Leaf | Node a (ABC a) (ABC a) deriving Show

search :: Ord a => a -> ABC a -> Bool
search _ Leaf = False
search x (Node y l r) | x == y = True
                      | x < y = search x l
                      | otherwise = search x r

-- Nu suporta sistemul varianta aceasta:
-- data Ord a => ABC' a = Leaf' | Node' a (ABC' a) (ABC' a) deriving Show

-- search' :: a -> ABC' a -> Bool
-- search' _ Leaf' = False
-- search' x (Node' y l r) | x == y = True
--                       | x < y = search' x l
--                       | otherwise = search' x r

-- Clasa de tipuri seamana putin cu interfetele din Java (sau cu clasele abstracte din C++)
