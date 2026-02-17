patratPerfect :: Int -> Bool
patratPerfect n = patratPerfectAux n 1

patratPerfectAux :: Int -> Int -> Bool
patratPerfectAux n k = if k > n then
                         False
                       else if k * k == n then
                         True
                       else
                         patratPerfectAux n (k + 1)

penultim :: [a] -> a
penultim [x1,x2] = x1
penultim (x1:xs) = penultim xs

penultim' :: [a] -> a
penultim' (x:xs) = if length xs == 1 then
                     x
                   else
                     penultim' xs

data Arb = Vid | Nod Int Arb Arb deriving (Show, Eq)
count :: Arb -> Int
count Vid = 0
count (Nod _ l r) = 1 + count l + count r

countAux' :: Arb -> Int -> Int
countAux' Vid acc = acc
countAux' (Nod _ l r) acc = countAux' r (countAux' l acc)

countAux :: [Arb] -> Int -> Int
countAux [] acc = acc
countAux (Vid:xs) acc = countAux xs acc
countAux (Nod _ l r:xs) acc = countAux (l:r:xs) (acc + 1)

count' :: Arb -> Int
count' x = countAux [x] 0

t1 = Nod 17 Vid Vid
t2 = Nod 13 t1 t1
t3 = Nod 7 t2 t1

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (hd:tl) = f hd : map' f tl

-- ganditi cum as putea face map tail-recursive?
-- map' :: (a -> b) -> [a] -> [b]
-- map' f [] = []
-- map' f (hd:tl) = f hd : map' f tl

data Nat = Zero | Succ Nat deriving (Show, Eq, Ord)

-- x, y ∈ N, y != 0:    ∃a, 0 <= b < y  a.i. x = y * a + b 

subtract' :: Nat -> Nat -> Nat
subtract' Zero _ = Zero
subtract' x Zero = x
subtract' (Succ x) (Succ y) = subtract' x y

lt :: Nat -> Nat -> Bool
lt Zero (Succ _) = True
lt _ Zero = False
lt (Succ x) (Succ y) = lt x y

lte :: Nat -> Nat -> Bool
lte Zero _ = True
lte (Succ _) Zero = False
lte (Succ x) (Succ y) = lte x y

lte' :: Nat -> Nat -> Bool
lte' x y = subtract' x y == Zero

-- quotientRemainder' :: Nat -> Nat -> (Int, Int)
-- convert :: Nat -> Int

quotientRemainder :: Nat -> Nat -> (Nat, Nat)
quotientRemainder x y = if lt x y then
                           (Zero, x)
                        else -- x >= y
                           let x' = subtract' x y in
                           let (a, b) = quotientRemainder x' y in
                             (Succ a, b)
qr :: Nat -> Nat -> (Nat, Nat)
qr x y = if x < y then
           (Zero, x)
         else -- x >= y
           let x' = subtract' x y in
           let (a, b) = qr x' y in
             (Succ a, b)

data Expr = Const Integer | Var String | Minus Expr | Plus Expr Expr |
  Mult Integer Expr deriving (Eq)

simpl :: Expr -> Expr
simpl (Const c) = Const c
simpl (Var v) = Var v
simpl (Minus e) = Minus (simpl e)
simpl (Plus e1 e2) = Plus (simpl e1) (simpl e2)
simpl (Mult c e) = if c < 0 then
                     Minus (simpl (Mult (-c) e))
                   else if c == 0 then
                     Const 0
                   else if c == 1 then
                     simpl e
                   else
                     Plus (simpl e) (simpl (Mult (c - 1) e))


esteSiLogic :: (Bool -> Bool -> Bool) -> Bool
esteSiLogic f = f True True && not (f True False) && not (f False True) &&
               not (f False False)

contineSiLogic :: [Bool -> Bool -> Bool] -> Bool
contineSiLogic [] = False
contineSiLogic (hd:tl) = esteSiLogic hd || contineSiLogic tl


instance Show Expr where
  show (Const c) = show c
  show (Var v) = v
  show (Minus e) = "(-" ++ (show e) ++ ")"
  show (Plus e1 e2) = "(" ++ show e1 ++ ") + (" ++ show e2 ++ ")"
  show (Mult c e) = "(" ++ show c ++ " * (" ++ show e ++ "))"

-- - > * > +
-- showsPrec :: Int -> a -> String -> String
