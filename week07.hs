-- (1) Polymorphism in Haskell

-- In Haskell, there are two
-- types of polymorphism:
-- (1) parametric polymorphism
-- (2) ad-hoc polymorphism

-- Polymorphism = there is a type variable in the signature of a
-- function

-- (1) Parametric polymorphism = the same implementation of the
-- function is used for any particular type;

-- (2) Ad-hoc polymorphism = there can be different implementations,
-- depending on the particular type.


data List a = Empty | Cons a (List a) deriving (Show, Eq)

-- (1) This is an example of parametric polymorphism:
len :: List a -> Int
--         ^^^
--      type variable, hence polymorphic
len Empty = 0
len (Cons hd tl) = 1 + len tl

-- Exactly the same code runs no matter what particular type is used
-- for "a".

-- The code is "blind", it cannot see anything about values of type
-- "a", so it treats them uniformly.

-- (2) Ad-hoc polymorphism

-- The code that is executed depends on the particular type that is
-- used for "a".

qs :: Ord a => [a] -> [a]
--       ^^^
--    constraint ---> the code that is executed
--                    depends on the particular type "a"
--                    that is used
--                    = ad-hoc polymorphism
qs [] = []
qs (hd:tl) = qs (filter (<=hd) tl) ++ [hd] ++ qs (filter (>hd) tl)

data Dow = Mon | Tue | Wed deriving (Show, Eq)

instance Ord Dow where
  (<=) Mon _ = False 
  (<=) Tue Mon = True
  (<=) Tue _ = False
  (<=) Wed Wed = False
  (<=) Wed _ = True

-- If a type variable is not constrained --> parametric polymorphism
-- If a type variable is constrained --> ad-hoc polymorphism

--                parametric polymorphism
--                       vvvvvvvvvvv
ex :: Ord a => a -> a -> b -> b -> b
--       ^^^
--     ad-hoc polymorphism
ex x y u v = if x < y then u else v







mistery1 :: a -> b
mistery1 x = mistery1 x

mistery2 :: a -> b
mistery2 _ = undefined

-- mistery :: a -> b

-- There are only two possible implementations
-- for the function mistery:
-- (1) it loops forever for any input
-- (2) it is partial, in the sense that
--     it is not defined for any input

-- If "mistery 42" loops forever,
-- then "mistery True" will also loop forever.

-- The behavior is uniform no matter the types "a", "b".


-- mistery :: a -> a
-- (1) identity function, (2) loop forever, (3) undefined for all inputs
mistery3 :: a -> a
mistery3 x = x

-- If "mistery 42" is "42", then "mistery True = True".

-- Example 3.

-- mistery :: [a] -> a
-- Guarantee:
--   mistery l is definitely an element of l

-- Guarantee: if the list is empty, then:
--   mistery [] definitely is: (1) undefined or (2) loops forever

-- (1) it can return any element in the list
-- (2) head
-- (3) last
-- (4) the function returning the 7th element in the list (undefined or loop forever if 7th element does not exist)
-- (5) can return the 2nd to last element (undefined or loop forever if 2nd to last element does not exist)
-- (6) can return different index elements depending on the length of
-- the list

-- (7) if mistery [1, 2, 3] returns 2, then mistery [True, False, True] = False

mistery4 :: [a] -> a
mistery4 list = if length list > 5 then
                  head list
                else
                  head (tail list)


-- Free theorems!

-- Theorem: if mistery [1, 2, 3] = 2 then mistery ['a', 'b', 'c'] = 'b'.


-- "How to turn a failure into a list
--  of successes"

r :: [x] -> [x]
r = reverse
-- what do you think "r" could do?
-- (1) could return the empty list
-- (2) same list (identity on lists)
-- (3) undefined/infinite (depending on length)
-- (4) a slice of the list
-- (5) shuffle the list (rearrange the list)
--     (for example, the "reverse" function)

-- r satisfies the following theorem:

-- for any types a, b,
-- for any list l :: [a],
-- for any total function f :: a -> b,
-- r (map f list) = map f (r list)

-- assume f is a total function
f :: Int -> Bool
f x = x `mod` 2 == 0

-- map :: (a -> b) -> [a] -> [b]

-- Could be useful: in compiler optimization.

-- in ad-hoc polymorphism, there are no (so many) free theorems
-- qs :: Ord a => [a] -> [a]
-- qs [] = []
-- qs (hd:tl) = qs (filter (<=hd) tl) ++ [hd] ++ qs (filter (>hd) tl)

-- compiler, behind the hood, conceptually:
-- qs :: (a -> a -> Bool) -> [a] -> [a]
--       ^^^^^^^^^^^^^^^^
--    additional parameter with the functions in the class

-- (2) Lazy evaluation


-- Teacher told you:

-- assignment statement:
-- x = e;
-- When the computer "sees" the assignment
-- statement "x = e;", it first evaluates
-- "e" and then writes the value into
-- the memory location "x".

-- function calls:
-- f(e1, e2, ..., en)
-- When the computer "sees" the function
-- call "f(e1, e2, ..., en)", it first
-- evaluates e1, e2, ..., en and then
-- runs the body of the function f replacing
-- the formal parameters by the values
-- computed.

f1 :: Integer -> Integer -> Integer
f1 x y = 42

fibo :: Int -> Integer
fibo 0 = 0
fibo 1 = 1
fibo x = fibo (x - 1) + fibo (x - 2)

a :: Int
a = let x = fibo 100 in 2

f2 :: Integer -> Integer -> Integer
f2 x y = if x > 2 then y else x

-- lazy evaluation means that an
-- expression is only evaluated when
-- its value is needed

-- in fact, behind the hood:
-- when you have some variable v :: T
-- the system does not store the value
-- of v, but it stores some code (thunk) that
-- can compute v when needed.

ite :: Bool -> a -> a -> a
ite True x y = x
ite False x y = y

-- disadvantage for lazy evaluation:
--   very difficult to reason about
--   the running time of a function

-- in Haskell: critical to simulate
--   imperative computations