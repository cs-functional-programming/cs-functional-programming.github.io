-- structuri de date funcționale
-- Fermoar (Zipper)

-- 1. Motivatie

data Tree = Empty | Nod Int Tree Tree deriving (Eq, Show)

t1 :: Tree
t1 = Nod 7 (Nod 3 (Nod 100 Empty Empty) Empty) (Nod 18 (Nod 6 Empty Empty) (Nod 5 Empty Empty))

{-
In C:
Node t1 = ...;
t1->left->left->value = 1;
-}

{-
7
  - 3
    - 100
         - Empty
         - Empty
    - Empty
  - 18
      - 6
         - Empty
         - Empty
      - 5
         - Empty
         - Empty
-}

-- t2 =  -- din nodul 100 sa fac 1

t2 :: Tree
t2 = Nod 7 (Nod 3 (Nod 1 Empty Empty) Empty) (Nod 18 (Nod 6 Empty Empty) (Nod 5 Empty Empty))

data Directie = L | R deriving (Show, Eq)

type Pozitie = [Directie] 

takeSubTree :: Tree -> Pozitie -> Tree
takeSubTree t [] = t
takeSubTree Empty (_:_) = error "Nu pot urma directia"
takeSubTree (Nod x left right) (L:dirs) = takeSubTree left dirs
takeSubTree (Nod x left right) (R:dirs) = takeSubTree right dirs

takeValue :: Tree -> Pozitie -> Int
takeValue Empty _ = error "Nu pot urma directia"
takeValue (Nod x _ _) [] = x
takeValue (Nod x left right) (L:dirs) = takeValue left dirs
takeValue (Nod x left right) (R:dirs) = takeValue right dirs

changeValue :: Tree -> Pozitie -> Int -> Tree
changeValue Empty _ _ = error "Nu pot schimba o valoare in subarborele vid"
changeValue (Nod x left right) [] newValue = Nod newValue left right
changeValue (Nod x left right) (L:dirs) newValue = Nod x (changeValue left dirs newValue) right
changeValue (Nod x left right) (R:dirs) newValue = Nod x left (changeValue right dirs newValue)


-- changeValue :: Tree -> Pozitie -> Int -> Tree
{-
In C:
Node t1 = ...;
t1->left->left->value = 1;
O(1)

In Haskell:
changeValue t1 [L, L] newValue e mai ineficient
-}

{-
Din 100 sa fac 1 si apoi din 3 sa fac 13

In C:
Node t1 = ...;
n = t1->left->left;
n->value = 1;
n = n->parent;
n->value = 13;

In Haskell:
ghci> (changeValue (changeValue t1 [L, L] 1) [L] 13)
(changeValue (changeValue t1 [L, L] 1) [L] 13)

-}


-- Zipper: o structura de date care tine minte nu numai arborele, ci si o pozitie curenta in arbore,
--         iar "modificarile" din arbore de langa pozitia curenta = O(1)
--         pastrand un stil de programare functional

type Zipper = (Tree, Pozitie)

updateZipper :: Zipper -> Int -> Zipper
updateZipper (t, pozitie) newValue = (changeValue t pozitie newValue, pozitie)

goLeft :: Zipper -> Zipper
goLeft (t, pozitie) = (t, pozitie ++ [L])

goRight :: Zipper -> Zipper
goRight (t, pozitie) = (t, pozitie ++ [R])

goUp :: Zipper -> Zipper
goUp (t, pozitie) = (t, take (length pozitie - 1) pozitie)

getVal :: Zipper -> Int
getVal (t, pozitie) = takeValue t pozitie

initZipper = (t1, [])
