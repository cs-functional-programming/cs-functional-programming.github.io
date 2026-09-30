import GHC.Float

-- Neoficial: add este o functie cu 2 argumente (x si y) care intoarce un Int
-- Oficial: add primeste un singur argument (x :: Int) si intoarce
--          o functie care, la randul ei, primeste un argument (y :: Int)
--                          si intoarce x + y
-- Exemplu: add 13 intoarce o functie care creste argumentul ei cu 13
add :: Int -> Int -> Int
-- "->" este un operator asociativ la dreapta
-- add :: (Int -> (Int -> Int))
add x y = x + y


exemplu :: Int
exemplu = add 13 7

-- parserul: juxtapunerea este operator asociativ la stanga
--      cand citeste "add 13 7"  ->  "((add 13) 7)"

-- sintaxa de apel a unei functii (neoficial):
--     numele functie
--     spatiu
--     primul argument
--     spatiu
--     al doilea argument
--     spatii ...
--     ultimul argument
-- oficial:
--     scriu numele functiei
--     scriu spatiu
--     scriu argumentul



-- "add" este o functie de ordin superior
-- (adica o functie care primeste ca argument sau intoarce ca rezultat o alta functie)

add' :: Int -> (Int -> Int)
add' x y = x + y







mistery = add 7

-- add 7 este o functie care are un argument si intoarce argumentul + 7

mistery_13 = add 13

-- add 13 este o functie care are un argument si intoarce argumentul + 13

mistery' :: Int -> Int
mistery' x = x + 7

{-
1) Functiile Haskell au cel mult un argument
2) "->" este un operator de tipuri asociativ la dreapta
3) aplicatia (juxtapunerea, " ") este asociativ la stanga
-}


functie1 :: (Int -> Int) -> Int
functie1 a = 13

functie2 :: (Int -> Int) -> Int
functie2 a = a 17

-- functie1 / functie2 este o functie de ordin superior


add3 :: (Int -> (Int -> (Int -> Int)))
add3 x y z = x + y + z

functie3 :: (Int -> Int) -> Int -> Int
functie3 f = f

functie4 :: (Int -> Int) -> Int -> Int
functie4 f x = f (x * 2)

mult :: Int -> Int -> Int
mult x y = x * y

functie5 :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)
functie5 f g x = f (g x)

functie6 :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)
functie6 f g = f . g

functie7 :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)
functie7 = (.)

-- In cele mai multe limbaje de programare, functiile pot sa primeasca
-- mai multe argumente.

-- In Haskell, cel mult un argument.

-- Sunt doua variante prin care pot "emula" functii cu mai multe argumente.

-- Varianta I (functii curried):
-- functii de ordin superior, ca mai sus

-- Varianta II (functii uncurried):
addv2 :: (Int, Int) -> Int
addv2 (x, y) = x + y


curry' :: ((Int, Int) -> Int) -> (Int -> (Int -> Int))
curry' f x y = f (x, y)

addv3 = curry' addv2

uncurry' :: (Int -> (Int -> Int)) -> ((Int, Int) -> Int)
uncurry' f (x, y) = f x y

addv4 = uncurry' addv3

add_point :: [Float] -> [Float]
add_point [] = []
add_point (hd:tl) = (hd + 1.0) : (add_point tl)

add_percentage :: [Float] -> [Float]
add_percentage [] = []
add_percentage (hd:tl) = (hd * 1.1) : (add_percentage tl)

process_grades :: (Float -> Float) -> [Float] -> [Float]
process_grades f [] = []
process_grades f (hd:tl) = (f hd) : (process_grades f tl)

inc_1 :: Float -> Float
inc_1 x = x + 1.0

add_10perc :: Float -> Float
add_10perc x = x * 1.1

add_point' = process_grades inc_1
add_percentage' = process_grades add_10perc

-- inc_1' :: Float -> Float
-- inc_1' =  \ x -> x + 1.0
-- add_10perc' :: Float -> Float
-- add_10perc' = \ x -> x * 1.1

add_point'' = process_grades (\ x -> x + 1.0)
add_percentage'' = process_grades (\ x -> x * 1.1)


names = [ "Stefan", "Andrei", "Sebastian" ]

process_names :: [String] -> [String]
process_names [] = []
process_names (hd:tl) = ("Hello, " ++ hd ++ "!") : (process_names tl)

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (hd:tl) = (f hd) : (map' f tl)

cine_a_trecut :: [(String, Float)] -> [String]
cine_a_trecut [] = []
cine_a_trecut ((nume, nota):tl) = if nota >= 5 then
                                    nume : cine_a_trecut tl
                                  else
                                    cine_a_trecut tl

proiectie :: [(String, Float)] -> [String]
proiectie = map (\(x, y) -> x)

cine_a_trecut' :: [(String, Float)] -> [String]
cine_a_trecut' note = map (\(x, y) -> x) (filter (\(x, y) -> y >= 5) note)

cine_a_trecut'' :: [(String, Float)] -> [String]
cine_a_trecut'' = (map fst) . (filter (\(_, y) -> y >= 5))


count :: [Float] -> Int
count [] = 0
count (_:tl) = 1 + count tl

suma :: [Float] -> Float
suma [] = 0
suma (hd:tl) = hd + suma tl

media :: [Float] -> Float
media note = (suma note) / (int2Float (count note))


reduce :: [a] -> (a -> b -> b) -> b -> b
reduce [] f a = a
reduce (hd:tl) f a = f hd (reduce tl f a)

count' :: [Float] -> Int
count' note = reduce note (\_ c -> c + 1) 0

suma' :: [Float] -> Float
suma' note = reduce note (\x y -> x + y) 0.0

media' :: [Float] -> Float
media' note = (suma' note) / (int2Float (count' note))

reduce' [] f a = a
reduce' (hd:tl) f a = reduce' tl f (f hd a)



reduce'' :: (b -> a -> b) -> b -> [a] -> b
reduce'' _ init [] = init
reduce'' f init (hd:tl) = reduce'' f (f init hd) tl

reduce''' :: (a -> b -> b) -> b -> [a] -> b
reduce''' _ init [] = init
reduce''' f init (hd:tl) = f hd (reduce''' f init tl)



{-
foldr (-) 4 [ 10, 50, 100 ]
56

4 - (10 - (50 - 100))


ghci> foldl (-) 4 [ 10, 50, 100 ]
foldl (-) 4 [ 10, 50, 100 ]
-156

(((4 - 10) - 50) - 100) = (-6 - 50) - 100 = -156
-}


applyFunctions :: [Int -> Int] -> Int -> Int
applyFunctions [] x = x
applyFunctions (hd:tl) x = applyFunctions tl (hd x)

qs :: (a -> a -> Bool) -> [a] -> [a]
qs p [] = []
qs p (hd:tl) = (qs p (filter (p hd) tl)) ++ [hd] ++ (qs p (filter (\x -> not (p hd x)) tl))
