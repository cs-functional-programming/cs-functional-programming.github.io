{-
enum Dow
{
  Mon,
  Tue,
  Wed,
  Thu,
  Fri,
  Sat,
  Sun
};
-}
data Dow = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving Show

next :: Dow -> Dow
next Mon = Tue
next Tue = Wed
next Wed = Thu
next Thu = Fri
next Fri = Sat
next Sat = Sun
next Sun = Mon

data Logic = Fals | Adevarat deriving Show

silogic :: Logic -> Logic -> Logic
silogic Adevarat Adevarat = Adevarat
silogic _ _ = Fals

data Rezultat = Invalid | Valid Int deriving Show

data GRezultat a = GInvalid | GValid a deriving Show

mydiv :: Int -> Int -> Rezultat
mydiv x 0 = Invalid
mydiv x y = Valid (div x y)

aduna3 :: Rezultat -> Int
aduna3 Invalid = 42
aduna3 (Valid x) = x + 3

data Lista = Nil | Cons Int Lista deriving Show

count :: Lista -> Int
count Nil = 0
count (Cons hd tl) = 1 + count tl

suma :: Lista -> Int
suma Nil = 0
suma (Cons hd tl) = hd + suma tl

impare :: Lista -> Lista
impare Nil = Nil
impare (Cons hd tl) = if even hd then
                        impare tl
                      else
                        Cons hd (impare tl)

incrlist :: Lista -> Lista
incrlist Nil = Nil
incrlist (Cons hd tl) = Cons (hd + 1) (incrlist tl)

data GLista a = GNil | GCons a (GLista a) deriving Show

gcount :: GLista a -> Int
gcount GNil = 0
gcount (GCons _ tl) = 1 + gcount tl

ghead :: GLista Int -> Int
ghead GNil = -1
ghead (GCons hd tl) = hd

ghead' :: GLista Int -> Rezultat
ghead' GNil = Invalid
ghead' (GCons hd tl) = Valid hd

ghead'' :: GLista a -> GRezultat a
ghead'' GNil = GInvalid
ghead'' (GCons hd tl) = GValid hd

ghead''' :: GLista a -> Maybe a
ghead''' GNil = Nothing
ghead''' (GCons hd tl) = Just hd

ghead'''' :: [a] -> Maybe a
ghead'''' [] = Nothing
ghead'''' (hd : tl) = Just hd

convert :: [a] -> GLista a
convert [] = GNil
convert (hd : tl) = GCons hd (convert tl)

convert' :: GLista a -> [a]
convert' GNil = []
convert' (GCons hd tl) = hd : (convert' tl)

data Pair = PairCons Int Int deriving Show

prima :: Pair -> Int
prima (PairCons x y) = x

doua :: Pair -> Int
doua (PairCons x y) = y

data GPair a b = GPairCons a b  deriving Show

gprima :: GPair a b -> a
gprima (GPairCons x y) = x

gdoua :: GPair a b -> b
gdoua (GPairCons x y) = y

{-

data Exp = Const Int | Add Exp Exp | Sub Exp Exp | Mul Exp Exp | Minus Exp deriving Show

eval :: Exp -> Int
eval (Const x) = x
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
eval (Minus e) = -(eval e)
-}

data Exp = Var String | Const Int | Add Exp Exp | Sub Exp Exp | Mul Exp Exp | Minus Exp | Pow Exp Exp deriving Show

eval :: Exp -> Int
eval (Var _) = 7 -- TODO: fix later
eval (Const x) = x
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
eval (Pow e1 e2) = (eval e1) ^ (eval e2)
eval (Minus e) = -(eval e)

egal :: Exp -> Exp -> Bool
egal (Const x) (Const y) = x == y
egal (Var x) (Var y) = x == y
egal (Add e1 e2) (Add e1' e2') = (egal e1 e1') && (egal e2 e2')
egal (Mul e1 e2) (Mul e1' e2') = (egal e1 e1') && (egal e2 e2')
egal (Sub e1 e2) (Sub e1' e2') = (egal e1 e1') && (egal e2 e2')
egal (Minus e) (Minus e') = egal e e'
egal (Pow e1 e2) (Pow e1' e2') = (egal e1 e1') && (egal e2 e2')
egal _ _ = False

simpl :: Exp -> Exp
simpl (Var x) = Var x
simpl (Const x) = Const x
simpl (Add e1 e2) = let e1' = simpl e1 in
                    let e2' = simpl e2 in
                    if egal e1' (Const 0) then
                      e2'
                    else if egal e2' (Const 0) then
                      e1'
                    else
                      Add e1' e2'
simpl (Mul e1 e2) = let e1' = simpl e1 in
                    let e2' = simpl e2 in
                    if egal e1' (Const 0) then
                      Const 0
                    else if egal e2' (Const 0) then
                      Const 0
                    else if egal e1' (Const 1) then
                      e2'
                    else if egal e2' (Const 1) then
                      e1'
                    else
                      Mul e1' e2'
simpl (Sub e1 e2) = let e1' = simpl e1 in
                    let e2' = simpl e2 in
                    if egal e1' (Const 0) then
                      Minus e2'
                    else if egal e2' (Const 0) then
                      e1'
                    else if egal e1' e2' then
                      Const 0
                    else
                      Sub e1' e2'
simpl (Minus e) = let e' = simpl e in
                  if egal e' (Const 0) then
                    Const 0
                  else
                    Minus e'
simpl (Pow e1 e2) = Pow (simpl e1) (simpl e2)
