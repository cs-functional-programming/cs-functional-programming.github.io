f :: Int -> Int
f x = x + 18

g :: Int -> Int -> Int
g x y = x + f y * 8

n :: Int
n = 10

g' :: Float -> Float
g' r = r * r * 3.14

sumn :: Int -> Int
sumn 0 = 0
sumn n = n + sumn (n - 1)

calcul :: Int -> Int
calcul x = if x > 7 then
             x - 18
           else
             x + 18

{-

for (int i = 2; i < x; ++i) {
  if (x % i == 0) {
    return false;
  }
}

-}

isPrimeAux :: Int -> Int -> Bool
isPrimeAux x i = if i >= x then
                   True
                 else if mod x i == 0 then
                   False
                 else
                   isPrimeAux x (i + 1)

isPrime :: Int -> Bool
isPrime x = isPrimeAux x 2



isPrime' :: Int -> Bool
isPrime' x = not (hasDivisorsBetween x 2 (x - 1))

-- hasDivisorsBetween :: Int -> Int -> Int -> Bool
-- hasDivisorsBetween x a b = if a > b then
--                              False
--                            else if mod x a == 0 then
--                              True
--                            else
--                              hasDivisorsBetween x (a + 1) b
-- garzi
hasDivisorsBetween :: Int -> Int -> Int -> Bool
hasDivisorsBetween x a b | a > b             = False
                         | x `mod` a == 0    = True
                         | otherwise         = hasDivisorsBetween x (a + 1) b

-- main :: Int -> Bool
-- main x = g x == g x

-- BACKTICK: `
-- nu este apostroful: '