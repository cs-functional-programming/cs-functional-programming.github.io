-- Clase de tipuri
-- Clase de tipuri != clasele din C++/Java/C#
-- Clasa de tipuri == multime de tipuri care au ceva in comun

data Dow = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Eq, Bounded, Enum)
{-
class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]
  {-# MINIMAL toEnum, fromEnum #-}
-}
instance Show Dow where
  show :: Dow -> String
  show Mon = "Luni"
  show Tue = "Marti"
  show Wed = "Miercuri"
  show Thu = "Joi"
  show Fri = "Vineri"
  show Sat = "Sambata"
  show Sun = "Duminica"

data Nat = Zero | Succ Nat deriving (Eq, Ord, Show) -- , Bounded)
--data Nat = Succ Nat | Zero deriving (Eq, Ord)

-- Atentie! Nat nu ar trebui sa fie instanta nici a clasei Num, nici a clasei Real

instance Num Nat where
  (+) Zero y = y
  (+) (Succ x) y = Succ (x + y)
  (*) Zero y = Zero
  (*) (Succ x) y = y + x * y
  abs x = x
  signum Zero = 0
  signum _ = 1
  negate x = Zero -- hack
  fromInteger 0 = Zero
  fromInteger x = Succ (fromInteger (x - 1))

instance Enum Nat where
  toEnum 0 = Zero
  toEnum x = Succ (toEnum (x - 1))
  fromEnum Zero = 0
  fromEnum (Succ x) = fromEnum x + 1

fromInteger' :: Integer -> Nat
fromInteger' 0 = Zero
fromInteger' x = Succ (fromInteger' (x - 1))

instance Real Nat where
  toRational x = toRational 0 -- (toInteger x) GHC.Real.:% 1

instance Integral Nat where
  toInteger Zero = 0
  toInteger (Succ x) = toInteger x + 1
  quotRem x y = (fromInteger' (quot (toInteger x) (toInteger y)),
                 fromInteger' (rem (toInteger x) (toInteger y)))

data Z = Neg Nat | Pos Nat -- deriving Ord -- ciudat -2 <= -3

-- Z ar merge instanta a clasei Num

instance Ord Z where
  (<=) (Pos x) (Pos y) = x <= y
  (<=) (Neg x) (Pos y) = True
  (<=) (Neg x) (Neg y) = x >= y
  (<=) (Pos x) (Neg y) = x == Zero && y == Zero

instance Eq Z where
  (==) (Pos x) (Pos y) = x == y
  (==) (Neg x) (Neg y) = x == y
  (==) (Pos x) (Neg y) = x == Zero && y == Zero
  (==) (Neg y) (Pos x) = x == Zero && y == Zero

  (/=) a b = not (a == b)
  -- (==) a b = not (a /= b)
  -- (/=) (Pos x) (Pos y) = x /= y
  -- (/=) (Neg x) (Neg y) = x /= y
  -- (/=) (Pos x) (Neg y) = x /= Zero || y /= Zero
  -- (/=) (Neg y) (Pos x) = x /= Zero || y /= Zero

-- clasa de tipuri din Haskell seamana cu interfetele din Java.

-- pot sa imi definesc propriile clase

class MyEq a where
  egal :: a -> a -> Bool
  egal x y = not (neegal x y)
  neegal :: a -> a -> Bool
  neegal x y = not (egal x y)
  {-# MINIMAL egal | neegal #-}

instance MyEq Nat where
  egal Zero Zero = True
  egal (Succ x) (Succ y) = egal x y
  egal _ _ = False
  -- neegal Zero Zero = False
  -- neegal (Succ x) (Succ y) = neegal x y
  -- neegal _ _ = True


-- Tipul "unit"

data Unit = U deriving (Show, Eq, Ord, Enum, Bounded)

g :: Unit -> Unit
g U = U

g' :: Unit -> Unit
g' U = g' U

h :: Unit -> Int
h U = 7

h' :: Unit -> Int
h' U = h' U + 2

f :: Int -> Unit
f x = U

f' :: Int -> Unit
f' 7 = U
f' x = f' x

-- seamana cu un tip din C/C++/Java/C#
-- Unit seamana cu void

{-

void f()
{
  printf("asdf");
}

-}


-- Kind-uri

--         un kind este pentru un tip de date
-- ceea ce este un tip pentru o valoare

-- cel mai simplu kind este "*"
-- Constraint
-- "->"

-- sort :: [Int] -> [Int]
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = (sort (filter (<=x) xs)) ++ [x] ++ (sort (filter (>x) xs))

-- dezavantajele claselor de tipuri

-- in traducerea in asm: sort primeste un parametru suplimentar

-- un tip poate sa faca parte dintr-o clasa de tipuri "intr-un singur mod"
-- De exemplu:
-- "asdf" < "thth"
-- sortez in ordine invers alfabetica
-- reverse (sort [ ... ])
-- sortez in ordine naturala
-- ["asdf20", "asdf100"]
-- definesc o instanta Ord String unde "asdf20" < "asdf100"
