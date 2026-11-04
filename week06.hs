-- TYPECLASSES
-- CLASE DE TIPURI

-- almost nothing similar to CLASSES IN C++/Java/C#
-- In Haskell: type class = set of types that have something in common (we will see what)
--                          class of types

-- data Dow = Mon | Tue | Wed deriving Show
-- ghci> Mon == Tue
-- Error: No instance for (Eq Dow) arising from a use of ‘==’

-- ghci> :type (==)
-- :type (==)
-- (==) :: Eq a => a -> a -> Bool
--        ^^^^^^
--        type constraint: the type "a" should be a member of the typeclass "Eq"


-- The typeclass "Eq" is the set of all types whose values can be compared for equality.

-- data Dow = Mon | Tue | Wed deriving (Show, Eq) -- Is only Eq: no way to convert Dow into String


{-
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS
-}

-- Really: you should think of typeclasses as sets of types that share some function declartions.
-- In particular: Eq is the set of all types whoses values you can compare for equality.

-- What do you think the typeclass Show is all about?
-- Show is the typeclass of types whose values can be converted into Strings.



-- data Dow = Mon | Tue | Wed deriving Eq

-- instance Show Dow where
--   show Mon = "Luni"
--   show Tue = "Marti"
--   show Wed = "Miercuri"
  
data Nat = Zero | Succ Nat deriving (Show, Eq)

-- data Z = UnaryPlus Nat | UnaryMinus Nat deriving (Show, Eq)
-- Issue: "deriving Eq" builds a function that checks for equality of representations
--                      not semantic equality

-- In such a case, you want your own instance for Eq

data Z = UnaryPlus Nat | UnaryMinus Nat deriving Show

instance Eq Z where
  --                               equality on Nat    (x :: Nat, y :: Nat, x == y)
  --                                 vvvvvv
  (==) (UnaryPlus x) (UnaryPlus y) = x == y
  (==) (UnaryMinus x) (UnaryMinus y) = x == y
  (==) (UnaryMinus x) (UnaryPlus y) = x == Zero && y == Zero 
  (==) (UnaryPlus x) (UnaryMinus y) = x == Zero && y == Zero

{-
week06.hs:53:57: error:
    Duplicate instance declarations:
      instance Eq Z -- Defined at week06.hs:53:57
      instance Eq Z -- Defined at week06.hs:55:10

One weakness of type classes in Haskell: a type can be an instance of a class in only one way.
You cannot have two instances for the same class and same type.
-}


-- data Dow = Mon | Tue | Wed deriving (Eq, Bounded, Enum, Ord, Show)



{-
instance Bounded Bool -- Defined in ‘GHC.Enum’
instance Bounded Char -- Defined in ‘GHC.Enum’
instance Bounded Int -- Defined in ‘GHC.Enum’
instance Bounded Word -- Defined in ‘GHC.Enum’
-}

-- Bounded is the class of types whose values have a lower bound (minBound) and an upper bound (maxBound).

-- Enum is the class of types whose values have predecessors and successors

-- succ is a partial function for Dow
-- succ Wed is not defined

-- `div` is also a partial function (not defined when the second argument is zero)

-- Typically: you want to avoid partial functions (you want functions to be total)
-- A partial function can throw an exception during evaluation (you have no control over this (white lie)).
-- It is better to have total functions that return Maybe

-- At some point: some asked if there is a better way to define nextDay
-- nextDay :: Dow -> Dow
-- nextDay Wed = Mon
-- nextDay x = succ x

-- Ord is the class of types whoses values can be ordered (<=).
sort :: Ord a => [a] -> [a]
sort [] = []
sort (hd:tl) = sort (filter (<=hd) tl) ++ [hd] ++ sort (filter (>hd) tl)

-- Can we assign a type constraint to an algebraic data type
-- Not in Haskell, we cannot.
data BST a = Leaf | Node (BST a) a (BST a)
-- data Ord a => BST a = Leaf | Node (BST a) a (BST a)
--     ^^^^^^^^^^
--    type constraints are not allowed in datatype definition

-- By convention, all constraints go into the functions, not into the datatype
search :: Ord a => a -> BST a -> Bool
search _ Leaf = False
search x (Node l v r) = if x == v then
                          True
                        else if x < v then
                          search x l
                        else
                          search x r
                          
-- insert :: Ord a => a -> BST a -> BST a


data Dow = Mon | Tue | Wed deriving Show

-- instance Eq Dow where
--   -- Mon == Mon = True
--   -- Tue == Tue = True
--   -- Wed == Wed = True
--   -- _ == _ = False
--   -- Mon /= Mon = False
--   -- Tue /= Tue = False
--   -- Wed /= Wed = False
--   -- _ /=_ = True
  
-- instance Eq Dow where
--   (==) Mon Mon = True
--   (==) Tue Tue = True
--   (==) Wed Wed = True
--   (==) _ _ = False
  
-- There is nothing special about the type classes
-- discussed above. They are all defined in the
-- Standard Library, not in the language itself.

-- User can define their own typeclasses

class MyEq a where
  eq :: a -> a -> Bool
  eq x y = not (x `neq` y)
  neq :: a -> a -> Bool
  neq x y = not (x `eq` y)
  {-# MINIMAL eq | neq #-}

instance MyEq Dow where
  -- eq Mon Mon = True
  -- eq Tue Tue = True
  -- eq Wed Wed = True
  -- eq _ _ = False
  -- neq Mon Mon = False
  -- neq Tue Tue = False
  -- neq Wed Wed = False
  -- neq _ _ = True


-- Can we also enforce a maximum set of functions in a typeclass?

-- Not to my knowledge.
-- But you can mimic this by doing the following:
-- class MyEq a where
--   eq :: a -> a -> Bool

-- neq :: MyEq a => a -> a -> Bool
-- neq x y = not (x `eq` y)
