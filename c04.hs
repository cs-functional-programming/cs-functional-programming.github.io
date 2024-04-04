f :: Int -> Int -> Int
f x y = x + y

f1 :: Int -> (Int -> Int)
f1 x y = x + y
-- f1 primeste la intrare un numar (sa-i spunem x) si intoarce
-- o functie care incrementeaza argumentul sau cu x.

g1 = f1 7
g1' = f 7
-- g1 este functia care are un argument si intoarce un numar cu 7 mai mare decat
-- argumentul

g2 = f1 10
g2' = f 10
-- g2 este functia care incrementeaza argumentul cu 10 unitati

--
-- alternativ:

f'' = \ x y -> x + y

g :: String -> String -> String
g x y = x ++ y

h :: String -> String -> String
h x y = y ++ x

-- lambda notatie (λ-notație)

f' = \ (x :: Int) -> x + 3

flip' :: (a -> a -> a) -> (a -> a -> a)
flip' f = \ x y -> f y x

h' = flip' g

sum3 :: Int -> (Int -> (Int -> Int))
--sum3 x y z = x + y + z
-- sum3 = \ x y z -> x + y + z
sum3 = \ x -> (\ y -> (\ z -> x + y + z))

aa = aa

v :: [Int] -> [Int]
v [] = []
v (hd : tl) = (hd + 1) : (v tl)

w :: [Int] -> [Int]
w [] = []
w (hd : tl) = (hd * 2) : (w tl)

xyz :: (a -> b) -> ([a] -> [b])
xyz f [] = []
xyz f (hd : tl) = (f hd) : (xyz f tl)

v' = xyz (\x -> x + 1)
w' = xyz (\x -> x * 2)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (hd : tl) = if p hd then
                        hd : (filter' p tl)
                      else
                        (filter' p tl)

-- reduce :: [Int] -> Int
-- reduce [] = 0
-- reduce (hd : tl) = hd + (reduce tl)

-- reduce :: (Int -> Int -> Int) -> Int -> [Int] -> Int
-- reduce f a [] = a
-- reduce f a (hd : tl) = f hd (reduce f a tl)

reduce :: (a -> b -> b) -> b -> [a] -> b
reduce f a [] = a
reduce f a (hd : tl) = f hd (reduce f a tl)


-- curring si uncurring (de la Curry)

-- varianta "curried"
sum2 :: Int -> Int -> Int
sum2 x y = x + y

-- varianta "uncurried"
sum2' :: (Int, Int) -> Int
sum2' (x, y) = x + y

uncurry' :: ((a, b) -> c) -> (a -> b -> c)
uncurry' f x y = f (x, y)

curry' :: (a -> b -> c) -> ((a, b) -> c)
curry' f (x, y) = f x y

-- NB: uncurry' = curry
--     curry' = uncurry
