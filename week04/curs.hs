data ABC = Empty | Nod Integer ABC ABC deriving Show

t1 :: ABC
t1 = Empty

t2 :: ABC
t2 = Nod 2 Empty Empty

t3 :: ABC
t3 = Nod 7 t2 (Nod 6 Empty Empty)

t4 :: ABC
t4 = Nod 6 t2 (Nod 7 Empty Empty)

t5 :: ABC
t5 = Nod 12 t4 Empty

minim :: ABC -> Integer
minim (Nod x Empty r) = x
minim (Nod x l r) = minim l
-- minim este o functie partial definita (functie partiala)

minim' :: ABC -> Maybe Integer
minim' Empty = Nothing
minim' (Nod x Empty r) = Just x
minim' (Nod x l r) = minim' l

maxim :: ABC -> Integer
maxim (Nod x l Empty) = x
maxim (Nod x l r) = maxim r
-- maxim este o functie partial definita (functie partiala)

maxim' :: ABC -> Maybe Integer
-- presupune ca argumentul este chiar un ABC
maxim' Empty = Nothing
maxim' (Nod x l Empty) = Just x
maxim' (Nod x l r) = maxim' r

smallerThan :: ABC -> Integer -> Bool
-- smallerThan t v = toate valorile din t sunt mai mici decat v
smallerThan Empty _ = True
smallerThan (Nod x l r) v = x < v && smallerThan l v && smallerThan r v

smallerThan' :: ABC -> Integer -> Bool
-- smallerThan' t v = pp ca t este ABC, smallerThan t v
smallerThan' Empty _ = True
smallerThan' (Nod x l r) v = x < v && smallerThan r v

greaterThan :: ABC -> Integer -> Bool
greaterThan Empty _ = True
greaterThan (Nod x l r) v = x > v && greaterThan l v && greaterThan r v

isABC :: ABC -> Bool
isABC Empty = True
isABC (Nod x l r) =
  smallerThan l x &&
  greaterThan r x &&
  isABC l &&
  isABC r

search :: ABC -> Integer -> Bool
search Empty _ = False
search (Nod x l r) v = if v == x then
                         True
                       else if v < x then
                         search l v
                       else
                         search r v

search' :: ABC -> Integer -> Bool
search' Empty _ = False
search' (Nod x l r) v = case compare v x of
                          LT -> search' l v
                          EQ -> True
                          GT -> search' r v

insert :: ABC -> Integer -> ABC
insert Empty v = Nod v Empty Empty
insert (Nod x l r) v = if x == v then
                         Nod x l r
                       else if v < x then
                         Nod x (insert l v) r
                       else
                         Nod x l (insert r v)

t6 :: ABC
t6 = (Nod 7 (Nod 4 (Nod 2 Empty Empty) (Nod 5 Empty Empty)) (Nod 11 Empty Empty))

data Expr = Const Integer
          | Suma Expr Expr
          | Produs Expr Expr
          | Var String
          | Expo Expr Expr deriving Show

e1 :: Expr
e1 = Suma (Const 7) (Const 14)

e2 :: Expr
e2 = Produs (Const 3) (Suma (Const 7) (Const 14))

e3 :: Expr
e3 = Suma (Var "x") (Const 14)

e4 :: Expr
e4 = Produs (Var "y") (Suma (Const 7) (Const 14))


type Assignment = [ (String, Integer) ]

assignment :: Assignment
assignment = [ ("x", 7), ("y", 12), ("x", 3) ]

lookup' :: Assignment -> String -> Maybe Integer
lookup' [] _ = Nothing
lookup' ((var, val):tl) x = if x == var then
                             Just val
                           else
                             lookup' tl x

eval :: Expr -> Assignment -> Integer
eval (Var x) tau = case lookup' tau x of
                     Just val -> val
eval (Const c) tau = c
eval (Suma e1 e2) tau = (eval e1 tau) + (eval e2 tau)
eval (Produs e1 e2) tau = (eval e1 tau) * (eval e2 tau)
eval (Expo e1 e2) tau = (eval e1 tau) ^ (eval e2 tau)

-- 3 * x^2 + 7 ---> 6x

e5 :: Expr
e5 = Suma (Produs (Const 3) (Produs (Var "x") (Var "x"))) (Const 7)

derivata :: Expr -> Expr
derivata (Var x) = if x == "x" then
                     Const 1
                   else
                     Const 0
derivata (Const c) = Const 0
derivata (Suma e1 e2) = Suma (derivata e1) (derivata e2)
derivata (Produs e1 e2) = Suma (Produs (derivata e1) e2) (Produs e1 (derivata e2))
derivata (Expo e1 e2) = Const 0

simpl :: Expr -> Expr
simpl (Const c) = (Const c)
simpl (Var v) = (Var v)
simpl (Suma e1 e2) =
  let e1' = simpl e1 in
  let e2' = simpl e2 in
    case (e1', e2') of
      (Const c1, Const c2) -> (Const (c1 + c2))
      (Const 0, _) -> e2'
      (_, Const 0) -> e1'
      _ -> Suma e1' e2'
simpl (Produs e1 e2) =
  let e1' = simpl e1 in
  let e2' = simpl e2 in
    case (e1', e2') of
      (Const c1, Const c2) -> (Const (c1 * c2))
      (Const 0, _) -> Const 0
      (_, Const 0) -> Const 0
      (Const 1, _) -> e2'
      (_, Const 1) -> e1'
      _ -> Produs e1' e2'
simpl (Expo e1 e2) = Expo e1 e2
