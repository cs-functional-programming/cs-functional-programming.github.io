-- Algebraic Datatypes (ADT)
-- tipuri algebrice de date
-- do not mistake for abstract datatype

-- simplest ADT: an enumeration of all possible values
data Dow = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Eq)

nextDow :: Dow -> Dow
nextDow Mon = Tue
nextDow Tue = Wed
nextDow Wed = Thu
nextDow Thu = Fri
nextDow Fri = Sat
nextDow Sat = Sun
nextDow Sun = Mon

isWorkDay :: Dow -> Bool
isWorkDay Sat = False
isWorkDay Sun = False
isWorkDay _ = True

--       constructor  constructor
--           vvvvvvv    vvvvvvv
data MyBool = MyFalse | MyTrue deriving (Show, Eq)
--   ^^^^^^
-- name of type
--


--  "|" disjoint sum

negation :: MyBool -> MyBool
negation MyFalse = MyTrue
negation MyTrue = MyFalse


--
mydivision :: Integer -> Integer -> Integer
mydivision _ 0 = 0
mydivision x y = x `div` y

mydivision' :: Integer -> Integer -> Integer
mydivision' x y = if x < y then 0 else 1 + (mydivision' (x - y) y)

data EnhancedInteger = Un Integer | Nimic deriving (Show, Eq)
--                     ^^           ^^^^^
--                 constructor    constructor
-- Examples of values of type EnhancedInteger:
-- Nimic :: EnhancedInteger
-- Un 10 :: EnhancedInteger
-- Un 42 :: EnhancedInteger

mydivision'' :: Integer -> Integer -> EnhancedInteger
mydivision'' _ 0 = Nimic
mydivision'' x y = Un (x `div` y)

wasOkay :: EnhancedInteger -> Bool
wasOkay (Un _) = True
wasOkay Nimic = False

--                    of 2nd ctor   of 2nd ctor
--                       1st arg     2nd arg
--                         vvvvvvv vvvvvvv
data List    = Vida | Cons Integer List deriving (Show, Eq)
--   ^^^^^^^   ^^^^   ^^^^
---name of type 1st     2nd
--               ctor    ctor

count :: List -> Int
count Vida = 0
count (Cons _ tail) = 1 + count tail

sum' :: List -> Integer
sum' Vida = 0
sum' (Cons x tail) = x + sum' tail

sum'_aux :: List -> Integer -> Integer
sum'_aux Vida a = a
sum'_aux (Cons x tail) a = sum'_aux tail (x + a)

data Pair = MyPair Integer Integer deriving  (Show, Eq)

-- when a constructor has multiple arguments: multiplication


fst' :: Pair -> Integer
fst' (MyPair x y) = x

data BoolList = VidaBool | ConsBool Bool BoolList deriving (Show, Eq)

-- parametric algebraic datatype

--  'a' is a type variable (later on: can decide a = Integer / a = Bool)
--       parameter
--         vvv
data MyList a = Empty | MyCons a (MyList a) deriving (Show, Eq)
--   ^^^^^^
--  name of type

myCount :: MyList a -> Int
myCount Empty = 0
myCount (MyCons _ tail) = 1 + myCount tail


mydivision''' :: Integer -> Integer -> Maybe Integer
mydivision''' _ 0 = Nothing
mydivision''' x y = Just (x `div` y)

mysqrt :: Integer -> Maybe Integer
mysqrt x | x < 0 = Nothing
mysqrt x         = Just (ceiling (sqrt (fromInteger x)))

myFancyComputation1 = mydivision''' 10 3
myFancyComputation2 = mysqrt 100
myFancyComputation3 = mydivision''' 10 0
myFancyComputation4 = mysqrt (-10)

data FailureReason = DivisionByZero | SqrtOfNegative deriving (Show, Eq)

mydivision'''' :: Integer -> Integer -> Either FailureReason Integer
mydivision'''' _ 0 = Left DivisionByZero
mydivision'''' x y = Right (x `div` y)

mysqrt' :: Integer -> Either FailureReason  Integer
mysqrt' x | x < 0 = Left SqrtOfNegative
mysqrt' x         = Right (ceiling (sqrt (fromInteger x)))


myFancyComputation1' = mydivision'''' 10 3
myFancyComputation2' = mysqrt' 100
myFancyComputation3' = mydivision'''' 10 0
myFancyComputation4' = mysqrt' (-10)


--
data Exp = Const Integer | Var String | Add Exp Exp | Mul Exp Exp deriving (Show, Eq)

eval :: Exp -> Integer
eval (Const c) = c
eval (Var x) = 0 -- TODO: need better code here
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- y * 2 + x + 7

-- simpl x + 0     x
-- simpl x * 1     x

simpl :: Exp -> Exp
simpl (Const c) = Const c
simpl (Var x) = (Var x)
simpl (Add e1 e2) = let e1' = simpl e1 in
                    let e2' = simpl e2 in
                      case (e1', e2') of
                        (Const 0, _) -> e2'
                        (_, Const 0) -> e1'
                        (Const c1, Const c2) -> Const (c1 + c2)
                        _ -> Add e1' e2'
simpl (Mul e1 e2) = let e1' = simpl e1 in
                    let e2' = simpl e2 in
                      case (e1', e2') of
                        (Const 1, _) -> e2'
                        (_, Const 1) -> e1'
                        (Const c1, Const c2) -> Const (c1 * c2)
                        _ -> Mul e1' e2'



data BST = Leaf | Node Integer BST BST deriving (Show, Eq)

search :: BST -> Integer -> Bool
search Leaf _ = False
search (Node v left right) x = if x < v then
                                 search left v
                               else if x > v then
                                 search right v
                               else
                                 True

-- function names: small letter
-- type names, constructor names: capital letter
