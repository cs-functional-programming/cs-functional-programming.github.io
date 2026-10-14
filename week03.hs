-- ALGEBRAIC DATATYPES
-- TIPURI DE DATE ALGEBRICE
-- ADTs, not to be mistaken for ABSTRACT DATA TYPES

-- ADTs are a generalization of ENUM TYPES

{-
enum Dow { Mon, Tue, Wed, Thu, Fri, Sat, Sun };
-}

--keyword       (constructors (start with Capital letter))
----       vvv   vvv   vvv   vvv ...               
data Dow = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving Show
--   ^^^                                           ^^^^^^^^^^^^^
-- name of type (starts with a Capital letter)     magic incantation
--                                                 (explained later)

isWeekend :: Dow -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _ = False

nextDay :: Dow -> Dow -- not the best way to define nextDay
nextDay Mon = Tue
nextDay Tue = Wed
nextDay Wed = Thu
nextDay Thu = Fri
nextDay Fri = Sat
nextDay Sat = Sun
nextDay Sun = Mon

divide :: Int -> Int -> Int
divide x y = x `div` y

divide' :: Int -> Int -> Int
divide' x 0 = 0
divide' x y = x `div` y

divide'' :: Int -> Int -> Int
divide'' x 0 = -1
divide'' x y = x `div` y

--                      either an Int, or some other value
--                      that I cannot mistake for an integer
--                           vvvvvvv
-- divide''' :: Int -> Int -> Mistery

divide''' :: Int -> Int -> (Int, Bool) -- very good idea
divide''' x 0 = (42, False)
divide''' x y = (x `div` y, True)

--            a value representing that the computation failed
--              vvvvvvvv
data MaybeInt = NotAnInt | AnInt Int deriving Show
--                         ^^^^^ ^^^
--                 constructor    parameter to constructor
--  Analogy:
---                  box            content of box / envelope
--                  envelope

divide4 :: Int -> Int -> MaybeInt
divide4 x 0 = NotAnInt
divide4 x y = AnInt (x `div` y)

-- constructors can take arguments/parameters

-- divide 42 by some number and then add 5
interestingComputation :: Int -> Int
interestingComputation x = (42 `div` x) + 5

--interestingComputation' :: Int -> MaybeInt
--interestingComputation' x = (divide4 42 x) + 5    (type error)
--                            ^^^^^^^^^^^^^^  ^^^
--                               MaybeInt     Int

interestingComputation' :: Int -> MaybeInt
interestingComputation' x = case divide4 42 x of
                              NotAnInt -> NotAnInt
                              AnInt y -> AnInt (y + 5)

addNumberToMI :: MaybeInt -> Int -> MaybeInt
addNumberToMI NotAnInt _ = NotAnInt
addNumberToMI (AnInt x) y = AnInt (x + y)

interestingComputation'' :: Int -> MaybeInt
interestingComputation'' x = addNumberToMI (divide4 42 x) 5
-- there are some better ways to do the above



data Pair = P Int Int deriving Show

extractFst :: Pair -> Int
extractFst (P x y) = x

extractSnd :: Pair -> Int
extractSnd (P x y) = y

createPair :: Int -> Int -> Pair
createPair x y = P x y
--              ^^^
--          constructor


-- a resemblance between applying functions
-- and applying constructors



--                  head of the list
--                         vvv  
data Mistery = Done | Cons Int Mistery deriving Show
--                             ^^^^^^^
--                          tail of the list (itself a list)

-- examples of values of type Mistery
-- Done :: Mistery
-- Cons 42 Done :: Mistery
-- Cons 7 (Cons 42 Done) :: Mistery
-- Cons 4 (Cons 7 (Cons 42 Done)) :: Mistery

sumAll :: Mistery -> Int
sumAll Done = 0
sumAll (Cons x y) = x + sumAll y

eraseLast :: Mistery -> Mistery -- let's get back to this
eraseLast Done = Done           -- we want a better way
eraseLast (Cons x Done) = Done
eraseLast (Cons x y) = Cons x (eraseLast y)

search :: Mistery -> Int -> Bool
search Done x = False
search (Cons x y) x' = if x == x' then
                         True
                       else
                         search y x'

-- Good exercise: addInTheMiddle




data MaybeMistery = NotAMistery | AMistery Mistery deriving Show

-- eraseLast :: Mistery -> Mistery 
-- eraseLast Done = Done -- <---- hides the mess under the rug          
-- eraseLast (Cons x Done) = Done
-- eraseLast (Cons x y) = Cons x (eraseLast y)

eraseLast' :: Mistery -> MaybeMistery 
eraseLast' Done = NotAMistery
eraseLast' (Cons x Done) = AMistery Done
eraseLast' (Cons x y) = case eraseLast' y of
                          NotAMistery -> NotAMistery -- code unreachable
                          AMistery y' -> AMistery (Cons x y')


-- data MaybeMistery = NotAMistery | AMistery Mistery deriving Show
-- data MaybeInt = NotAnInt | AnInt Int deriving Show

-- parametric ADT
-- a little bit like the C++ code: template<T> Option<T> { ... }
data Option a = None | Some a deriving Show
--         ^^^
--       type variable
--       "a" stands for a type
-- Instead of MaybeMistery, I will be using the type "Option Mistery"
--            MaybeInt                               "Option Int"

eraseLast'' :: Mistery -> Option Mistery 
eraseLast'' Done = None
eraseLast'' (Cons x Done) = Some Done
eraseLast'' (Cons x y) = case eraseLast'' y of
                          None -> None -- code unreachable
                          Some y' -> Some (Cons x y')

divide5 :: Int -> Int -> Option Int
divide5 x 0 = None
divide5 x y = Some (x `div` y)


eraseLast''' :: Mistery -> Maybe Mistery 
eraseLast''' Done = Nothing
eraseLast''' (Cons x Done) = Just Done
eraseLast''' (Cons x y) = case eraseLast''' y of
                            Nothing -> Nothing -- code unreachable
                            Just y' -> Just (Cons x y')

divide6 :: Int -> Int -> Maybe Int
divide6 x 0 = Nothing
divide6 x y = Just (x `div` y)

-- quick application:
data Exp = Const Int | Mult Exp Exp | Add Exp Exp | Var String deriving Show
-- more compact than Exp in C++/Java

eval :: Exp -> Int
eval (Const c) = c
eval (Mult e1 e2) = (eval e1) * (eval e2)
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Var x) = 0 -- some smarter here

-- any algebraic datatype is a tree (constructors are nodes of the tree)


divide7 :: Int -> Int -> Either Int String
divide7 x 0 = Right "division by zero"
divide7 x y = Left (x `div` y)
