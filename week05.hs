-- HIGHER-ORDER FUNCTIONS
-- FUNCȚII DE ORDIN SUPERIOR

-- In Haskell: functions are first-class citizens:
-- Anything that you can do with a typical value, you can also do with a function.

-- Let me show you how you can create in Haskell a function on-the-spot

-- We define the function "f".
f :: Integer -> Integer -> Integer
f x y = 2 * x + y

-- At some point, we need the function 3 * x + y
-- We want to create this function (3 * x + y) on the spot, not define it in the file.

-- ghci> :t (\x y -> 3 * x + y)
-- (\x y -> 3 * x + y) :: Num a => a -> a -> a

-- It does not even have a name.
-- Anonymous function.

g :: Integer -> Integer -> Integer
g = \x y -> 3 * x + y -- the syntax for defining functions "on-the-spot"
                      -- lambda-expression (we will get back to this)
                      -- (In C++: auto g = [](int x, int y) { return 3 * x + y; })

-- Can you have, in the language C, a function that creates a function on the spot
-- and returns it back?

-- You can have a function that creates an integer on the spot and returns it back.
{-
int compute(int x)
{
  return 3 * x + 7;
}
// In C++: ???
auto create_function()
{
  return [](int x) { return 3 * x + 7; }
}
-}

-- Let us write a function that computes not an integer, but a function.

compute_function x = \y -> 3 * x + y

h = compute_function 10

-- compute_function 10 = ...
--                 ^^^   ^^^
--                 int  first-order function

-- "h" is a first-order function, because it only operates on values.
-- h 100 = 130
--   ^^^   ^^^
--   int   int

-- What does "h" do? Adds its argument to 3 * 10.

-- Welcome to higher-order functions: functions that return other
-- functions as results or take other functions as arguments.
-- "compute_function" is a higher-order function
-- Why? Because it returns a function as a result

-- The functions in the language C are called first-order functions, because
-- they cannot return functions or take functions as arguments (except in some
-- restricted manner).

-- In fact, compute_function is a second-order function: because it
-- returns a first-order function.


-- Let us now build a function that takes a function as argument.

process_function func = \x -> func (x + 3)
--               ^^^^   ^^^^^^^^^^^^^^^^^^
--            function     function
--               here         here

-- What does "h" do? Adds its argument to 3 * 10.
-- h 100 = 130
h' = process_function h
-- What does "h'" do?
-- h' 100 =
-- (process_function h) 100 =
-- (\x -> h (x + 3)) 100 =
-- h (100 + 3) =
-- h 103 =
-- ... = 133


--           constructs a value from the function
--                  vvvvvv
use_function func = func 7
--           ^^^^  
--  takes a function
--  as an arugment

-- h adds 30 to its argument
-- h' adds 33 to its argument

v1 = use_function h
v2 = use_function h'

-- You should have been bothered by the syntax below:
--       #1st arg    #2nd arg     #result
--          vvvvvvv    vvvvvvv    vvvvvvv
addition :: Integer -> Integer -> Integer
-- Reasons to be bothered:
-- The arrow just before the #2nd argument is weird:
--   because you would expect a result after the arrow
--   and an argument before the arrow.
-- There is an asymmetry: #1st argument has no arrow before it.
-- Could have a better syntax for a two-argument function:
--    addition :: Integer, Integer -> Integer

-- Lie: in Haskell there are no two-argument functions.
-- Everything function in Haskell takes exactly one argument.

addition x y = x + y

-- In fact, addition' is a function taking
-- an Integer into a (function from Integers to Integers)
--addition' :: Integer -> Integer -> Integer
-- The arrow is right-associative.
-- t1 -> t2 -> t3 = t1 -> (t2 -> t3)
addition' :: Integer -> (Integer -> Integer)
addition' x y = x + y

-- The syntax for function calls.
-- What is the Haskell syntax for function calls?
-- function_name arg1 arg2 ... argn
-- function_name space space-separated arguments
-- In fact, as Haskell functions have exactly one argument,
-- function calls have the syntax:
-- function_name arg.

-- Function call (invisible syntax: space between function and arugment)
-- is left-associative.

-- Because "->" is right-associative and function call (space, " ") is
-- left-assoc we can pretend to have functions having multiple
-- arguments.

-- In fact, addition is a higher-order function that takes x ::
-- Integer as an argument and builds another function as result.  When
-- you type "addition 13 17", the system parses it as
-- "(addition 13) 17",
-- at evaluation time: conceptually builds a function that adds 13 to its argument
-- and calls it on 17.


add_42 = addition 42

compute_adder :: Integer -> (Integer -> Integer)
compute_adder x = (\v -> v + x)

add_100 :: Integer -> Integer
add_100 = compute_adder 100

add_7 :: Integer -> Integer
add_7 = compute_adder 7

add_50 :: Integer -> Integer
add_50 = compute_adder 50

-- difference between Integer and Int
-- Int are machine integers (typically, on today's computers, -2^63 to 2^63 - 1)
-- Integer are mathematical integers (only limited by the ammount of memory you have)

data List = Empty | Cons Integer List deriving (Show, Eq)

data List' a = Empty' | Cons' Integer (List' a) deriving (Show, Eq)

sumList :: List -> Integer
sumList Empty = 0
sumList (Cons hd tl) = hd + sumList tl

list1 = Cons 42 (Cons 7 (Cons 53 Empty))

-- incValuesList :: List -> List
-- incValuesList Empty = Empty
-- incValuesList (Cons hd tl) = Cons (hd + 1) (incValuesList tl)

-- add7ValuesList :: List -> List
-- add7ValuesList Empty = Empty
-- add7ValuesList (Cons hd tl) = Cons (hd + 7) (add7ValuesList tl)

addValueToList :: Integer -> List -> List
addValueToList value Empty = Empty
addValueToList value (Cons hd tl) = Cons (hd + value) (addValueToList value tl)
incValuesList = addValueToList 1
add7ValuesList = addValueToList 7

-- need to multiply each element in a list by 2
-- mult2List :: List -> List
-- mult2List Empty = Empty
-- mult2List (Cons hd tl) = Cons (hd * 2) (mult2List tl)

-- need to multiply each element in a list by 7
multList :: Integer -> List -> List
multList value Empty = Empty
multList value (Cons hd tl) = Cons (hd * value) (multList value tl)
mult2List = multList 2

-- Dimitrie Cocîrță: 2p bonus points for noticing wrong recursive call due to copy-paste
-- Cristian Luchin: 1p bonus point for noticing wrong recursive call due to copy-paste


--                1st order              1st order
--             vvvvvvvvvvvvvvvvvvvv    vvvvvvvvvvvvv
processList :: (Integer -> Integer) -> (List -> List)
-- ghci> :t processList
-- :t processList
-- processList :: (Integer -> Integer) -> List -> List
-- the first pair of brackets are important
-- ||| if they were missing:
-- ||| processList :: Integer -> Integer -> List -> List
-- ||| processList :: Integer -> (Integer -> (List -> List))
processList processInt Empty = Empty
processList processInt (Cons hd tl) = Cons (processInt hd) (processList processInt tl)

addValueToListOld :: Integer -> (List -> List)
addValueToListOld value = processList (\x -> x + value)

multListOld :: Integer -> (List -> List)
multListOld value = processList (\x -> x * value)


-- Welcome to the well-known map operation.

onlyOdd :: List -> List
onlyOdd Empty = Empty
onlyOdd (Cons hd tl) = if hd `mod` 2 == 0 then
                         onlyOdd tl
                       else
                         Cons hd (onlyOdd tl)

onlyNonnegative :: List -> List
onlyNonnegative Empty = Empty
onlyNonnegative (Cons hd tl) = if hd < 0 then
                                 onlyNonnegative tl
                               else
                                 Cons hd (onlyNonnegative tl)

keep :: (Integer -> Bool) -> List -> List
keep predicate Empty = Empty
keep predicate (Cons hd tl) = if predicate hd then
                                 Cons hd (keep predicate tl)
                               else
                                 keep predicate tl

-- Welcome to the well-known function filter.

-- Puzzle: what does mistery do
mistery :: (Integer -> Integer -> Integer) -> (Integer -> Integer -> Integer)
mistery f = \x y -> f y x

myDiv = mistery div

mistery' :: (Integer -> Integer) -> (Integer -> Integer) -> (Integer -> Integer)
mistery' f g x = f (g x)

f1 = mistery' (\x -> x + 10) (\x -> x * 2)

-- what does f1 do?

-- what does mistery' do? Function composition.

-- There is a built-in function composition operator called "." (infix).
