f x = x + 23

data Dow = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving Bounded

instance Show Dow where
  show Mon = "Luni"
  show Tue = "Marti"
  show Wed = "Miercuri"
  show Thu = "Joi"
  show Fri = "Vineri"
  show Sat = "Sambata"
  show Sun = "Duminica"

instance Eq Dow where
  -- (==) Mon Mon = True
  -- (==) Tue Tue = True
  -- (==) Wed Wed = True
  -- (==) Thu Thu = True
  -- (==) Fri Fri = True
  -- (==) Sat Sat = True
  -- (==) Sun Sun = True
  -- (==) _ _ = False
  (/=) Mon Mon = False
  (/=) Tue Tue = False
  (/=) Wed Wed = False
  (/=) Thu Thu = False
  (/=) Fri Fri = False
  (/=) Sat Sat = False
  (/=) Sun Sun = False
  (/=) _ _ = True

class MyEq a where
  egal :: a -> a -> Bool
  egal x y = not (neegal x y)
  neegal :: a -> a -> Bool
  neegal x y = not (egal x y)
  {-# MINIMAL egal | neegal #-}

instance MyEq Dow where
  egal Mon Mon = True
  egal Tue Tue = True
  egal Wed Wed = True
  egal Thu Thu = True
  egal Fri Fri = True
  egal Sat Sat = True
  egal Sun Sun = True
  egal _ _ = False
  -- neegal Mon Mon = False
  -- neegal Tue Tue = False
  -- neegal Wed Wed = False
  -- neegal Thu Thu = False
  -- neegal Fri Fri = False
  -- neegal Sat Sat = False
  -- neegal Sun Sun = False
  -- neegal _ _ = True

data Nat = Zero | Succ Nat deriving Eq

convert :: Nat -> Int
convert Zero = 0
convert (Succ n) = 1 + (convert n)

instance Show Nat where
  show n = show (convert n)

data Intreg = Negativ Nat | Pozitiv Nat

instance Show Intreg where
  show (Negativ n) = "-" ++ (show n)
  show (Pozitiv n) = "+" ++ (show n)

instance Eq Intreg where
  (==) (Pozitiv Zero) (Negativ Zero) = True
  (==) (Negativ Zero) (Pozitiv Zero) = True
  (==) (Pozitiv x) (Pozitiv y) = x == y
  (==) (Negativ x) (Negativ y) = x == y
  (==) _ _ = False

data MyNat = MySucc MyNat | MyZero deriving (Eq, Show)

instance Ord MyNat where
  -- (<=) MyZero _ = True
  -- (<=) (MySucc _) MyZero = False
  -- (<=) (MySucc x) (MySucc y) = (<=) x y
  
  -- (<=) MyZero _ = True
  -- (<=) (MySucc _) MyZero = False
  -- (<=) (MySucc x) (MySucc y) = x <= y
  -- MyZero <= _ = True
  -- (MySucc _) <= MyZero = False
  -- (MySucc x) <= (MySucc y) = x <= y
  compare MyZero MyZero = EQ
  compare MyZero (MySucc _) = LT
  compare (MySucc _) MyZero = GT
  compare (MySucc x) (MySucc y) = compare x y

sort :: Ord a => [a] -> [a]
sort [] = []
sort (hd:tl) = (sort (filter (<=hd) tl)) ++ [hd] ++ (sort (filter (>hd) tl))

instance Enum Dow where
  fromEnum Mon = 0
  fromEnum Tue = 1
  fromEnum Wed = 2
  fromEnum Thu = 3
  fromEnum Fri = 4
  fromEnum Sat = 5
  fromEnum Sun = 6
  toEnum 0 = Mon
  toEnum 1 = Tue
  toEnum 2 = Wed
  toEnum 3 = Thu
  toEnum 4 = Fri
  toEnum 5 = Sat
  toEnum 6 = Sun


-- tipul de date Unit

data Unit = U deriving (Show, Eq, Ord, Enum, Bounded)

g :: Unit -> Unit
g _ = U

g' :: Unit -> Unit
g' U = U

g'' :: Unit -> Unit
g'' _ = g'' U

gg :: () -> ()
gg _ = ()

gg' :: () -> ()
gg' () = ()

gg'' :: () -> ()
gg'' _ = gg'' ()

h :: Int -> ()
h 13 = ()
h _ = h 14

h' :: () -> Int
h' () = 13

h'' :: () -> Int
h'' () = h'' ()

-- clasa Functor
-- clasa Foldable

impartire :: Int -> Int -> Int
impartire x y = x `div` y

putere :: Int -> Int -> Int
putere x 0 = 1
putere x p = x * putere x (p - 1)

impartire' :: Int -> Int -> Maybe Int
impartire' x 0 = Nothing
impartire' x y = Just (x `div` y)

putere' :: Int -> Int -> Maybe Int
putere' x p | p < 0 = Nothing
putere' 0 0 = Nothing
putere' x 0 = Just 1
putere' x p = case putere' x (p - 1) of
                Nothing -> Nothing
                Just v -> Just (v * x)

data Exc = DivByZero | NotDefined | NotRepr deriving Show

impartire'' :: Int -> Int -> Either Exc Int
impartire'' x 0 = Left DivByZero
impartire'' x y = Right (x `div` y)

putere'' :: Int -> Int -> Either Exc Int
putere'' x p | p < 0 = Left NotRepr
putere'' 0 0 = Left NotDefined
putere'' x 0 = Right 1
putere'' x p = case putere'' x (p - 1) of
                Left x -> Left x
                Right v -> Right (v * x)

putere''' :: Int -> Int -> Either Exc Int
putere''' x p | p < 0 = Left NotRepr
putere''' 0 0 = Left NotDefined
putere''' x 0 = Right 1
putere''' x p = fmap (*x) (putere''' x (p - 1))

res1 = impartire' 10 3
res2 = impartire'' 10 3
res3 = impartire' 10 0
res4 = impartire'' 10 0

res1p1 = case res1 of
           Nothing -> Nothing
           Just x -> Just (x + 1)
res1p1' = fmap (+1) res1

data Arb a = Leaf | Nod a (Arb a) (Arb a) deriving Show

a1 = Nod 3 (Nod 1 Leaf Leaf) (Nod 6 Leaf Leaf)

instance Functor Arb where
  fmap f Leaf = Leaf
  fmap f (Nod x l r) = Nod (f x) (fmap f l) (fmap f r)

instance Foldable Arb where
  -- foldr :: (a -> b -> b) -> b -> Arb a -> b
  foldr f i Leaf = i
  foldr f i (Nod x l r) = foldr f (foldr f (f x i) l) r
  