f :: Int -> Int
f x = x + 13

-- pure functional programming = referential transparency

add :: Int -> Int -> Int
add x y = x + y

-- ghci is a REPL

-- compute the product of the first n positive integers
-- 1 x 2 x ... x n
prod :: Int -> Int
prod 0 = 1
prod n = n * (prod (n-1))

-- prod 2 = 2 * prod 1 = 2 * (1 * prod 0) = 2 * (1 * 1) = 2

-- prod 2 = 2 * prod 1 = 2 * (1 * prod 0) = 2 * (1 * 1) = 2 * 1 = 2
--         \___/        \________/         
--         stack        stack (2 frames)     (3 frames)
--        (1 frame)

{-
for (int i = 1; i <= n; ++i) {
  result *= i;
}
-}

-- prod' :: Int -> Int
-- prod' n = n * (prod' (n-1))
-- prod' 0 = 1

-- prod' 2 = 2 * prod' 1 = 2 * (1 * prod' 0) = 2 * 1 * 0 * prod' (-1) = ...

-- tail-call optimization

--          (accumulator)
--         n      a 
prod'' :: Int -> Int -> Int
prod'' 0 a = a
prod'' n a = prod'' (n-1) (a * n) -- tail-call

-- prod'' 5 1 = prod'' 4 5 = prod'' 3 (5 * 4) = prod'' 2 (5 * 4 * 3) =
--          prod'' 1 (5 * 4 * 3 * 2) = prod'' 0 (5 * 4 * 3 * 2 * 1) = 5x4x3x2x1


fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n - 2)

fibo_aux :: Int -> Int -> Int -> Int
fibo_aux 0 a b = a
--fibo_aux 1 a b = b
fibo_aux n a b = fibo_aux (n-1) b (a+b)

fibo_aux' :: Int -> Int -> Int -> Int
fibo_aux' 0 a b = a
fibo_aux' 1 a b = b
fibo_aux' n a b = fibo_aux' (n-1) b (a+b)

-- prove ∀ n >= 0, a, b : fibo_aux' n a b = fibo_aux n a b
-- induction on n:
-- Base Case 1. n = 0:
-- prove ∀ a, b : a = fibo_aux' 0 a b = fibo_aux 0 a b = a (a = a by refl)
-- Base Case 2. n = 1:
-- prove ∀ a, b : b = fibo_aux' 1 a b = fibo_aux 1 a b = fibo_aux 0 b (a+b) = b
-- Inductive case (n >= 2):
-- prove ∀ a, b : fibo_aux' (n-1) b (a+b) =
--                        fibo_aux' n a b =
--                         fibo_aux n a b =
--                 fibo_aux (n-1) b (a+b)
-- by induction hypothesis fibo_aux' (n-1) b (a+b) = fibo_aux (n-1) b (a+b).

fibo' :: Int -> Int
fibo' n = fibo_aux n 0 1


-- prod :: Int -> Int
-- prod 0 = 1
-- prod n = n * (prod (n-1))
-- prod'' :: Int -> Int -> Int
-- prod'' 0 a = a
-- prod'' n a = prod'' (n-1) (a * n) -- tail-call
-- show, using equational reasoning, that prod and prod'' compute the same value

-- prove ∀ n, a . a * prod n = prod'' n a
-- by induction on n:
-- Base Case: n = 0
--   a * prod 0 = a * 1
--   prod'' 0 a = a
--   ✓
-- Inductive Case: n > 0
--   Induction Hypothesis: ∀ a. a * prod (n-1) = prod'' (n-1) a
--   a * prod n = a * n * (prod (n-1)) = (a * n) * prod (n - 1) =(IH) =
--                                     = prod'' (n-1) (a * n)
--   prod'' n a = prod'' (n-1) (a * n)
--  ✓

-- Can we write a function that has two outputs?
-- Answer 1. No, we cannot.
--f1 :: Int -> Int -> Int -> Int
--   \_________________/  \___/
--      inputs           single output

-- take an angle as input
-- output the point on the unit circle at that angle

xCoord :: Float -> Float
xCoord angle = cos (pi * angle / 180)

yCoord :: Float -> Float
yCoord angle = sin (pi * angle / 180)

coords :: Float -> (Float, Float)
coords angle = (xCoord angle, yCoord angle)

succ' :: Int -> Int
succ' x = x + 1

pred' :: Int -> Int
pred' x = x - 1

-- extended Euclidian algorithm:
-- given x, y
-- standard algorithm: compute d = gcd(x, y)
-- extended algorithm: also computes a, b s.t. a * x + b * y = d
-- gcd_extended :: Int -> Int -> (Int, Int, Int)


fst_3uple :: (Int, Int, Int) -> Int
fst_3uple (x, _, _) = x
