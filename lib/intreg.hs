module Lib.Intreg where

data Intreg = Cons Int deriving Show

-- adunare :: Intreg -> Intreg -> Intreg
-- adunare (Cons x) (Cons y) = Cons (x + y)

add :: Intreg -> Intreg -> Intreg
add (Cons x) (Cons y) = Cons (x + y)
