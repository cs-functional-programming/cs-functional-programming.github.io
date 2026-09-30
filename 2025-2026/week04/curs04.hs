-- Tipuri algebrice de date (Partea II)

data Dow = Monday | Tuesday | Wednesday

data EnhancedInteger = Nimic | Chiar Integer deriving Show
--                     ^^^^^   ^^^^^
--                       constructori

-- valorile de tip EnhancedInteger:
-- (1) Nimic
-- (2) Chiar 0
-- (3) Chiar 1
-- (4) Chiar 7
-- (5) Chiar (-47)
-- ...

impartire0 :: Integer -> Integer -> Integer
impartire0 x y = div x y

impartire :: Integer -> Integer -> EnhancedInteger
impartire x 0 = Nimic
impartire x y = Chiar (div x y)

data EnhancedString = Nimic' | Chiar' String deriving Show

primeleTreiCaractere :: String -> EnhancedString
primeleTreiCaractere (a:(b:(c:_))) = Chiar' [a,b,c]
primeleTreiCaractere _ = Nimic'

primeleTreiCaractere' :: String -> Maybe String
primeleTreiCaractere' (a:(b:(c:_))) = Just [a,b,c]
primeleTreiCaractere' _ = Nothing

impartire' :: Integer -> Integer -> Maybe Integer
impartire' x 0 = Nothing
impartire' x y = Just (div x y)


data MotivEroare = DivisionByZero | NegativeSqrt deriving Show

impartire'' :: Integer -> Integer -> Either MotivEroare Integer
impartire'' x 0 = Left DivisionByZero
impartire'' x y = Right (div x y)

calculInteresant :: Integer -> Integer -> Integer -> Maybe Integer
calculInteresant x y z = case impartire'' x y of
                           Left reason -> Nothing
                           Right result -> Just (result + z)

data BST = Empty | Node Integer BST BST deriving Show

{-

Frunzele la liceu erau etichetate.

O frunza "de la liceu" este reprezentata ca Node 3 Empty Empty

-}

t1 :: BST
t1 = Node 3 Empty Empty

t2 :: BST
t2 = Node 14 Empty Empty

t3 :: BST
t3 = Node 7 t1 t2

t4 :: BST
t4 = Node 20 t3 Empty

{-
          20
        /     \
       7        E
    /     \
   3      14
  / \     / \
 E   E   E   E
-}

s1 :: BST
s1 = Node 2 Empty Empty

s2 :: BST
s2 = Node 1 Empty Empty

s3 :: BST
s3 = Node 42 s1 s2

s4 :: BST
s4 = Node 10 s3 Empty

{-
          10
        /     \
      42        E
    /     \
   2       1
  / \     / \
 E   E   E   E
-}

-- search 42 s4 sa dea un rezultat gresit

search :: Integer -> BST -> Bool
search key Empty = False
search key (Node x l r) = if key == x then
                            True
                          else if key < x then
                            search key l
                          else -- key > x
                            search key r

allSmallerThan :: BST -> Integer -> Bool
allSmallerThan Empty key = True
allSmallerThan (Node x l r) key = x < key && allSmallerThan r key

allGreaterThan :: BST -> Integer -> Bool
allGreaterThan Empty key = True
allGreaterThan (Node x l r) key = x > key && allGreaterThan l key

isBST :: BST -> Bool
isBST Empty = True
isBST (Node x l r) = isBST l && isBST r && allSmallerThan l x && allGreaterThan r x

insert :: Integer -> BST -> BST
insert key Empty = Node key Empty Empty
insert key (Node x l r) = if key == x then
                            Node x l r
                          else if key < x then
                            Node x (insert key l) r
                          else
                            Node x l (insert key r)

{-
"Expresion problem"
-}

-- x * 2 + (y * 10 - z) * z + 27

data Expr = Const Integer | Var String | Plus Expr Expr | Mult Expr Expr | Minus Expr Expr | Impartire Expr Expr deriving Show

-- Modificare (1)
-- Vreau sa adaug un nou caz, pentru impartire
-- am adaugat un nou constructor
-- a trebuit sa modific toate functiile care procesau expresii (GREU)

-- Modificare (2)
-- Vreau sa adaug o noua functie, de simplificare
-- adaug functia si gata (SIMPLU)

e1 :: Expr
e1 = Plus (Plus (Mult (Var "x") (Const 2)) (Mult (Minus (Mult (Var "y") (Const 10)) (Var "z")) (Var "z"))) (Const 27)

prettyPrint :: Expr -> String
prettyPrint (Const c) = show c
prettyPrint (Var s) = s
prettyPrint (Plus e1 e2) = "(" ++ (prettyPrint e1) ++ " + " ++ (prettyPrint e2) ++ ")"
prettyPrint (Mult e1 e2) = "(" ++ (prettyPrint e1) ++ " * " ++ (prettyPrint e2) ++ ")"
prettyPrint (Minus e1 e2) = "(" ++ (prettyPrint e1) ++ " - " ++ (prettyPrint e2) ++ ")"
prettyPrint (Impartire e1 e2) = "(" ++ (prettyPrint e1) ++ " / " ++ (prettyPrint e2) ++ ")"

-- Exercitiu bonus pentru laborator: modificati prettyPrint astfel incat sa foloseasca numar cat mai mic de paranteze

data Map = EmptyMap | Bind String Integer Map deriving Show

m1 :: Map
m1 = Bind "x" 42 (Bind "y" 13 (Bind "z" 0 EmptyMap))

insertMap :: Map -> String -> Integer -> Map
insertMap map var val = Bind var val map

searchMap :: Map -> String -> Maybe Integer
searchMap EmptyMap var = Nothing
searchMap (Bind var val map) var' = if var == var' then
                                      Just val
                                    else
                                      searchMap map var'

simpl :: Expr -> Expr
simpl (Plus (Const c1) (Const c2)) = Const (c1 + c2)
simpl (Plus (Const 0) e2) = simpl e2
simpl (Plus e1 (Const 0)) = simpl e1
simpl (Mult (Const c1) (Const c2)) = Const (c1 * c2)
simpl (Mult (Const 1) e2) = simpl e2
simpl (Mult e1 (Const 1)) = simpl e1
simpl e = e


eval :: Expr -> Map -> Maybe Integer
eval (Const c) map = Just c
eval (Var var) map = searchMap map var
eval (Plus e1 e2) map = case (eval e1 map, eval e2 map) of
                          (Nothing, _) -> Nothing
                          (_, Nothing) -> Nothing
                          (Just v1, Just v2) -> Just (v1 + v2)
eval (Mult e1 e2) map = case (eval e1 map, eval e2 map) of
                          (Nothing, _) -> Nothing
                          (_, Nothing) -> Nothing
                          (Just v1, Just v2) -> Just (v1 * v2)
eval (Minus e1 e2) map = case (eval e1 map, eval e2 map) of
                          (Nothing, _) -> Nothing
                          (_, Nothing) -> Nothing
                          (Just v1, Just v2) -> Just (v1 - v2)
eval (Impartire e1 e2) map = case (eval e1 map, eval e2 map) of
                          (Nothing, _) -> Nothing
                          (_, Nothing) -> Nothing
                          (Just v1, Just 0) -> Nothing
                          (Just v1, Just v2) -> Just (v1 `div` v2)
